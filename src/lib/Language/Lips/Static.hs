module Language.Lips.Static where

--------------------
-- Global Imports --
import System.IO

-------------------
-- Local Imports --
import Language.Lips.Evaluator
import Language.Lips.Parser

----------
-- Code --

-- From a static file
runStatic :: FilePath -> IO ()
runStatic path = do
  contents <- readFile path
  mapM_ (\lip -> (putStrLn $ show $ eval lip) >> hFlush stdout) $ manyLips contents