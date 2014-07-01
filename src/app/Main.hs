module Main where

--------------------
-- Global Imports --
import Language.Lips.Static
import Language.Lips.REPL

import System.Environment

----------
-- Code --
main :: IO ()
main = do
  args <- getArgs

  case args of
    []        -> startRepl
    (x:[])    -> runStatic x
    otherwise -> putStrLn "Proper usage: lips [script filepath]"
