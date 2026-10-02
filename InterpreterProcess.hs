{-# LANGUAGE CPP #-}

-- Copyright 2026 Moritz Angermann <moritz@zw3rk.com>, zw3rk pte. ltd.
-- SPDX-License-Identifier: BSD-3-Clause

-- Process ownership for the proxy's --pipe mode.
module InterpreterProcess (withInterpreter) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import System.Exit
import System.IO
import System.IO.Error
import System.Process
#if !defined(mingw32_HOST_OS)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
#endif

data Interpreter = Interpreter
  { childInput :: Handle
  , childOutput :: Handle
  , childProcess :: ProcessHandle
#if !defined(mingw32_HOST_OS)
  , childGroup :: Maybe ProcessID
#endif
  }

-- The callback marks Shutdown before writing it to the interpreter. Protocol
-- work stays on the calling thread, where GHC's INT/QUIT handlers can reach it.
withInterpreter :: [String] -> (Handle -> Handle -> IO () -> IO ()) -> IO ()
withInterpreter args action = E.mask $ \restore -> do
  parent <- myThreadId
  withTermHandler parent $ do
    child <- spawn args
    result <- E.try $ do
      shuttingDown <- newIORef False
      E.bracket
        (forkIOWithUnmask $ \unmask ->
          unmask (watchChild parent shuttingDown (childProcess child)))
        (E.uninterruptibleMask_ . killThread)
        (\_ -> restore $ do
          action (childInput child) (childOutput child)
            (atomicWriteIORef shuttingDown True)
          status <- waitForExit 50 (childProcess child)
          case status of
            Just ExitSuccess -> pure ()
            Just code -> E.throwIO (childFailure code)
            Nothing -> E.throwIO (userError "iserv-proxy: interpreter did not finish Shutdown"))
    cleanupResult <- E.try $ case result of
      Right () -> closePipes child
      Left _ -> abortInterpreter child
    finish result cleanupResult

spawn :: [String] -> IO Interpreter
spawn (command:args) = do
  let options = (proc command args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
#if !defined(mingw32_HOST_OS)
        , close_fds = True
        , create_group = True
#endif
        }
  (Just input, Just output, _, process) <- createProcess options
#if !defined(mingw32_HOST_OS)
  group <- getPid process
  let child = Interpreter input output process group
#else
  let child = Interpreter input output process
#endif
  (do
    mapM_ (\handle -> do
      hSetBuffering handle NoBuffering
      hSetBinaryMode handle True) [input, output]
    pure child) `E.onException` abortInterpreter child
spawn [] = E.throwIO (userError "iserv-proxy: --pipe requires a command")

-- Polling keeps a non-threaded RTS responsive. A descendant holding stdout
-- open must not prevent the proxy from noticing its immediate child's exit.
watchChild :: ThreadId -> IORef Bool -> ProcessHandle -> IO ()
watchChild parent shuttingDown process =
  watch `E.catch` forwardIOError
  where
    watch = do
      status <- getProcessExitCode process
      case status of
        Nothing -> threadDelay 100000 >> watch
        Just code -> do
          expected <- readIORef shuttingDown
          unless (expected && code == ExitSuccess) $
            throwTo parent (childFailure code)
    -- Deliberately stopping the watcher must not forward ThreadKilled.
    forwardIOError err = throwTo parent (err :: E.IOException)

-- At most 'attempts' 100ms waits; getProcessExitCode itself is non-blocking.
waitForExit :: Int -> ProcessHandle -> IO (Maybe ExitCode)
waitForExit attempts process = do
  status <- getProcessExitCode process
  case status of
    Nothing | attempts > 0 -> do
      threadDelay 100000
      waitForExit (attempts - 1) process
    _ -> pure status

-- No protocol worker holds these handles. Keep cleanup immune to a second
-- cancellation signal, using bounded polling rather than exception timeouts.
abortInterpreter :: Interpreter -> IO ()
abortInterpreter child = E.uninterruptibleMask_ $ do
  observed <- getProcessExitCode (childProcess child)
  reportExit observed
#if !defined(mingw32_HOST_OS)
  signalGroup child sigTERM
#else
  terminateProcess (childProcess child)
#endif
  stopped <- waitForExit 50 (childProcess child)
#if !defined(mingw32_HOST_OS)
  -- Remove descendants even when the immediate child has already exited.
  signalGroup child sigKILL
#else
  when (stopped == Nothing) $ terminateProcess (childProcess child)
#endif
  reaped <- case stopped of
    Just code -> pure (Just code)
    Nothing -> waitForExit 10 (childProcess child)
  case reaped of
    Nothing -> E.throwIO (userError "iserv-proxy: interpreter did not exit after termination")
    Just _ -> do
      -- Closing may flush an interrupted builder write. Terminate its readers
      -- first, so a child retaining stdin cannot block cleanup in hClose.
      closePipes child
      when (observed == Nothing) (reportExit reaped)

closePipes :: Interpreter -> IO ()
closePipes child = E.uninterruptibleMask_ $
  mapM_ closePipe [childInput child, childOutput child]
  where
    closePipe handle = hClose handle `E.catch` ignoreCloseError
    ignoreCloseError :: E.IOException -> IO ()
    ignoreCloseError _ = pure ()

reportExit :: Maybe ExitCode -> IO ()
reportExit (Just code@(ExitFailure _)) =
  hPutStrLn stderr ("iserv-proxy: interpreter exit status: " ++ show code)
reportExit _ = pure ()

#if !defined(mingw32_HOST_OS)
signalGroup :: Interpreter -> Signal -> IO ()
signalGroup child signal = forM_ (childGroup child) $ \group -> do
  let send = signalProcessGroup signal group
  send `E.catch` \err ->
    if isDoesNotExistError err then pure ()
#if defined(darwin_HOST_OS)
    else if isPermissionError err then do
      -- Darwin can return EPERM for a group whose leader is still a zombie.
      -- Reap it and retry; a real permission error must still be reported.
      _ <- waitForExit 10 (childProcess child)
      send `E.catch` \retryError ->
        unless (isDoesNotExistError retryError) (E.throwIO (retryError :: E.IOException))
#endif
    else E.throwIO (err :: E.IOException)
#endif

withTermHandler :: ThreadId -> IO a -> IO a
#if !defined(mingw32_HOST_OS)
withTermHandler parent action = E.bracket
  (installHandler sigTERM (Catch (throwTo parent (ExitFailure 1))) Nothing)
  (\previous -> void $ installHandler sigTERM previous Nothing)
  (\_ -> action)
#else
withTermHandler _ action = action
#endif

childFailure :: ExitCode -> E.IOException
childFailure code = userError ("iserv-proxy: interpreter terminated (" ++ show code ++ ")")

-- Keep a protocol or child failure when cleanup also reports an error.
finish :: Either E.SomeException () -> Either E.SomeException () -> IO ()
finish (Left failure) cleanupResult = do
  case cleanupResult of
    Left err -> hPutStrLn stderr ("iserv-proxy: cleanup failed: " ++ E.displayException err)
    Right () -> pure ()
  E.throwIO failure
finish (Right ()) cleanupResult = either E.throwIO pure cleanupResult
