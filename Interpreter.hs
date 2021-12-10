module Main where

import System.Environment
import IServ.Remote.Message
import IServ.Remote.Interpreter

verbose :: Bool
verbose = False

main :: IO ()
main = do
    [portStr, storagePath] <- getArgs
    let port = read portStr
    startInterpreter' verbose storagePath port
