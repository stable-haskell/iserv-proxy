{-# LANGUAGE ForeignFunctionInterface, GADTs, LambdaCase #-}
module IServ.Remote.Interpreter where

import Network.Socket

import IServ (serv)
import IServ.Remote.Message

import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when, forever)
import System.Directory
import System.FilePath (takeDirectory, (</>), dropTrailingPathSeparator,
                        isAbsolute, joinPath, splitPath)
import GHCi.ResolvedBCO

import Data.IORef
import GHCi.Message (Pipe(..), Msg(..), Message(..), readPipe, writePipe)

import Foreign.C.String

import Data.Binary
import GHC.Fingerprint (getFileHash)

import qualified Data.ByteString as BS

import Text.Printf
import System.Environment (getProgName)

trace :: String -> IO ()
trace s = getProgName >>= \name -> printf "[%20s] %s\n" name s

dropLeadingPathSeparator :: FilePath -> FilePath
dropLeadingPathSeparator p | isAbsolute p = joinPath (drop 1 (splitPath p))
                           | otherwise    = p

-- | Path concatenation that prevents a double path separator to appear in the
-- final path. "/foo/bar/" <//> "/baz/quux" == "/foo/bar/baz/quux"
(<//>) :: FilePath -> FilePath -> FilePath
lhs <//> rhs = dropTrailingPathSeparator lhs </> dropLeadingPathSeparator rhs
infixr 5 <//>

foreign export ccall startInterpreter :: Bool -> Int -> CString -> IO ()

-- | @startInterpreter@ is the exported interpreter function, that the
-- hosting application on the target needs to invoke to
-- start the interpreter process, and runs iserv.
startInterpreter :: Bool -> Int -> CString -> IO ()
startInterpreter verbose port s = do
  base_path <- peekCString s
  trace $ "DocRoot: " ++ base_path
  _ <- forkIO $ startInterpreter' verbose base_path (toEnum port)
  return ()

-- | @startInterpreter'@ provdes a blocking haskell interface, that
-- the hosting application on the target can use to start the
-- interpreter process.
startInterpreter' :: Bool -> String -> PortNumber -> IO ()
startInterpreter' verbose base_path port = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  let addrInfo = defaultHints {
          addrFamily = AF_INET,
          addrSocketType = Stream
        }
  sock <- openSocket addrInfo
  setSocketOption sock ReuseAddr 1

  ai:_ <- getAddrInfo (Just addrInfo) Nothing (Just $ show port)
  bind sock (addrAddress ai)
  listen sock 1

  actualPort <- socketPort sock
  putStrLn $ "Listening on port " ++ show actualPort

  forever $ do
    when verbose $ trace "Opening socket"
    pipe <- acceptSocket sock >>= socketToPipe
    when verbose $ trace "Starting serv"
    uninterruptibleMask $ serv verbose (hook verbose base_path pipe) pipe
    when verbose $ trace "serv ended"
    return ()

-- | The iserv library may need access to files, specifically
-- archives and object files to be linked. If ghc and the interpreter
-- are on the same host, this is trivial, as the underlying
-- filestorage is the same.  If, however, the interpreter does not run
-- on the same host, the filestorage is not identical and we
-- need to request data from the host where ghc runs on.
--
-- If we however already have the requested file we need to make
-- sure that this file is the same one ghc sees. Hence we
-- calculate the Fingerprint of the file and send it back to the
-- host for comparison. The proxy will then send back either @Nothing@
-- indicating that the file on the host has the same Fingerprint, or
-- Maybe ByteString containing the payload to replace the existing
-- file with.
handleLoad :: Pipe -> FilePath -> FilePath -> IO ()
handleLoad pipe path localPath = do
  exists <- doesFileExist localPath
  if exists
    then getFileHash localPath >>= \hash -> proxyCall (Have path hash) >>= \case
      Nothing -> return ()
      Just bs -> BS.writeFile localPath bs
    else do
      createDirectoryIfMissing True (takeDirectory localPath)
      resp <- proxyCall (Missing path)
      BS.writeFile localPath resp

  proxyCall Done
  where
    proxyCall :: (Binary a, Show a) => ProxyMessage a -> IO a
    proxyCall msg = do
      writePipe pipe (putProxyMessage msg)
      readPipe pipe get

-- | The hook we install in the @serv@ function from the
-- iserv library, to request archives over the wire.
hook :: Bool -> String -> Pipe -> Msg -> IO Msg
hook verbose base_path pipe m = case m of
  Msg (AddLibrarySearchPath p) -> do
    when verbose $ putStrLn ("Need Path: " ++ (base_path <//> p))
    createDirectoryIfMissing True (base_path <//> p)
    return $ Msg (AddLibrarySearchPath (base_path <//> p))
  Msg (LoadObj path) -> do
    when verbose $ putStrLn ("Need Obj: " ++ (base_path <//> path))
    handleLoad pipe path (base_path <//> path)
    return $ Msg (LoadObj (base_path <//> path))
  Msg (LoadArchive path) -> do
    handleLoad pipe path (base_path <//> path)
    return $ Msg (LoadArchive (base_path <//> path))
  -- when loading DLLs (.so, .dylib, .dll, ...) and these are provided
  -- as relative paths, the intention is to load a pre-existing system library,
  -- therefore we hook the LoadDLL call only for absolute paths to ship the
  -- dll from the host to the target.  On windows we assume that we don't
  -- want to copy libraries that are referenced in C:\ these are usually
  -- system libraries.
  Msg (LoadDLL path@('C':':':_)) -> do
    return m
  -- When building on nix the /nix/store paths use Z:
  Msg (LoadDLL path@('Z':':':_)) -> do
    return m
  Msg (LoadDLL path) | isAbsolute path -> do
    when verbose $ trace ("Need DLL: " ++ (base_path <//> path))
    handleLoad pipe path (base_path <//> path)
    return $ Msg (LoadDLL (base_path <//> path))
  _other -> return m

--------------------------------------------------------------------------------
-- socket to pipe briding logic.
socketToPipe :: Socket -> IO Pipe
socketToPipe sock = do
  hdl <- socketToHandle sock ReadWriteMode
  hSetBuffering hdl NoBuffering

  lo_ref <- newIORef Nothing
  pure Pipe{ pipeRead = hdl, pipeWrite = hdl, pipeLeftovers = lo_ref }

acceptSocket :: Socket -> IO Socket
acceptSocket = fmap fst . accept
