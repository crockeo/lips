module Language.Lips.REPL where

--------------------
-- Global Imports --
import System.IO

-------------------
-- Local Imports --
import Language.Lips.Evaluator
import Language.Lips.Parser

----------
-- Code --

-- Printing the REPL header
printHeader :: IO ()
printHeader = do
  putStrLn "--------------------"
  putStrLn "      lips REPL"
  putStrLn "Enter 'quit' to exit"
  putStrLn ""
  hFlush stdout

-- The REPL itself
repl :: IO ()
repl = do
  putStr "lips} "
  hFlush stdout

  line <- getLine

  case line of
    "quit" -> return ()
    other  -> do
      putStrLn $ show $ eval $ lips other
      hFlush stdout
      repl

-- Starting the REPL
startRepl :: IO ()
startRepl = printHeader >> repl