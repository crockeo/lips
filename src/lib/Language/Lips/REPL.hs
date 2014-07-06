module Language.Lips.REPL where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map

import Control.Monad.State

import System.IO

-------------------
-- Local Imports --
import Language.Lips.Evaluator
import Language.Lips.Parser
import Language.Lips.State

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
repl :: ProgramState ()
repl = do
  liftIO $ putStr "lips} "
  liftIO $ hFlush stdout

  line <- liftIO $ getLine

  case line of
    "quit" -> return ()
    other  -> do
      eval $ lips other
      liftIO $ hFlush stdout
      repl

-- Starting the REPL
startRepl :: IO ()
startRepl = do
  printHeader
  evalStateT repl $ Program { primitives = Map.fromList []
                            , variables  = Map.fromList []
                            }
