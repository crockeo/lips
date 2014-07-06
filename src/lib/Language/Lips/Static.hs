module Language.Lips.Static where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map

import Control.Monad.State

import System.IO

-------------------
-- Local Imports --
import Language.Lips.LanguageDef
import Language.Lips.Evaluator
import Language.Lips.Parser
import Language.Lips.State

----------
-- Code --

-- Running a list of LipsVals
runList :: [LipsVal] -> ProgramState ()
runList []     = return ()
runList (x:xs) = do
  eval x
  liftIO $ hFlush stdout

  runList xs

-- From a static file
runStatic :: FilePath -> IO ()
runStatic path = do
  contents <- readFile path
  evalStateT (runList $ manyLips contents) $ Program { primitives = Map.fromList []
                                                     , variables  = Map.fromList []
                                                     }
