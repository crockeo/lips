module Language.Lips.Evaluator where

--------------------
-- Global Imports --
import Control.Monad.State (liftIO)
import Control.Monad

-------------------
-- Local Imports --
import Language.Lips.LanguageDef
import Language.Lips.State
import Language.Lips.Base

----------
-- Code --

-- Applying a function
apply :: ProgramState Function -> [LipsVal] -> ProgramState LipsVal
apply psfn args = do
  fn <- psfn
  return $ fn args

-- New eval
eval :: LipsVal -> ProgramState LipsVal
eval (LList [LAtom "bind"   , LAtom name, val]) = eval val >>= (\x -> pushVariable name (\l -> x))  >> return lNull
eval (LList [LAtom "drop"   , LAtom name     ]) = dropVariable name              >> return lNull
eval (LList [LAtom "print"  , val            ]) = eval val >>= (liftIO . putStr   . show) >> return lNull
eval (LList [LAtom "println", val            ]) = eval val >>= (liftIO . putStrLn . show) >> return lNull
eval (LList [LAtom "quote"  , val            ]) = return val
eval (LList (LAtom name:args)                 ) = (sequence $ map eval args) >>= apply (getVariable name)
eval (LAtom name                              ) = apply (getVariable name) []
eval other                                      = return other
