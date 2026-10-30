module Main where

import           IServ.Remote.Interpreter (startInterpreter', startInterpreterStdio)
import           System.Environment (getArgs, getProgName)
import           System.Exit (die)

import Control.Monad (when)

main :: IO ()
main = getArgs >>= startSlave

dieWithUsage :: IO a
dieWithUsage = do
  prog <- getProgName
  die $ msg prog
 where
  msg name = "usage: " ++ name ++ " /path/to/storage PORT [-v] [--no-load-call]\n"
          ++ "       " ++ name ++ " /path/to/storage --stdio [-v] [--no-load-call]"

startSlave :: [String] -> IO ()
startSlave args0
  | "--help" `elem` args0 = dieWithUsage
  | "--stdio" `elem` args0 = do
      let rest = filter (`notElem` ["--stdio"]) args0
      (path, flags) <- case rest of
        arg0:flags -> return (arg0, flags)
        _          -> dieWithUsage

      let verbose = "-v" `elem` flags
          noLoadCall = "--no-load-call" `elem` flags

      when (any (`notElem` ["-v", "--no-load-call"]) flags)
        dieWithUsage

      startInterpreterStdio verbose noLoadCall path
  | otherwise = do
      (path, port, rest) <- case args0 of
        arg0:arg1:rest -> return (arg0, read arg1, rest)
        _              -> dieWithUsage

      let verbose = "-v" `elem` rest
          noLoadCall = "--no-load-call" `elem` rest

      when (any (`notElem` ["-v", "--no-load-call"]) rest)
        dieWithUsage

      startInterpreter' verbose noLoadCall path port
