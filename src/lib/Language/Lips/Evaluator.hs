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
newEval :: LipsVal -> ProgramState LipsVal
newEval (LList [LAtom "bind"   , LAtom name, val]) = newEval val >>= (\x -> pushVariable name (\l -> x))  >> return lNull
newEval (LList [LAtom "drop"   , LAtom name     ]) = dropVariable name              >> return lNull
newEval (LList [LAtom "print"  , val            ]) = newEval val >>= (liftIO . putStr   . show) >> return lNull
newEval (LList [LAtom "println", val            ]) = newEval val >>= (liftIO . putStrLn . show) >> return lNull
newEval (LList [LAtom "quote"  , val            ]) = return val
newEval (LList (LAtom name:args)                 ) = (sequence $ map newEval args) >>= apply (getVariable name)
newEval (LAtom name                              ) = apply (getVariable name) []
newEval other                                      = return other

-- Evaluating a LipsVal operation
eval :: LipsVal -> LipsVal
eval (LAtom func                ) = getPrimitive func []
eval (LList [LAtom "quote", val]) = val
eval (LList (LAtom func:args)   ) = getPrimitive func $ map eval args
eval other                        = other
