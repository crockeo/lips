module Language.Lips.Evaluator where

--------------------
-- Global Imports --
import Control.Monad.State (liftIO)
import Control.Monad

-------------------
-- Local Imports --
import Language.Lips.LanguageDef
import Language.Lips.Error
import Language.Lips.State
import Language.Lips.Base

----------
-- Code --

-- Binding a variable
eBind :: String -> LipsVal -> ProgramState (Error LipsVal)
eBind name val = do
  evaled <- eval val

  pushVariable name $ \l -> Success evaled
  return $ Success lNull

-- Dropping a variable
eDrop :: String -> ProgramState (Error LipsVal)
eDrop name = do
  exists <- hasVariable name

  case exists of
    False -> return $ Error VariableNotDefinedError
    True  -> do
      dropVariable name
      return $ Success lNull

-- Printing a variable
ePrint :: LipsVal -> ProgramState (Error LipsVal)
ePrint val = do
  eval val >>= liftIO . putStr   . show
  return $ Success lNull

-- Printing a variable with a newline
ePrintln :: LipsVal -> ProgramState (Error LipsVal)
ePrintln val = do
  eval val >>= liftIO . putStrLn . show
  return $ Success lNull

-- Applying a function
eApply :: String -> [LipsVal] -> ProgramState (Error LipsVal)
eApply name args = do
  mvar <- getVariableSafe name

  case mvar of
    Nothing  -> return $ Error VariableNotDefinedError
    Just var -> return $ var args

-- Safely evaluating things
safeEval :: LipsVal -> ProgramState (Error LipsVal)
safeEval (LList [LAtom "bind"   , LAtom name, val]) = eBind name val
safeEval (LList [LAtom "drop"   , LAtom name     ]) = eDrop name
safeEval (LList [LAtom "print"  , val            ]) = ePrint val
safeEval (LList [LAtom "println", val            ]) = ePrintln val
safeEval (LList [LAtom "quote"  , val            ]) = return $ Success val
safeEval (LList (LAtom name     : args           )) = eApply name args
safeEval (LAtom name                              ) = eApply name []
safeEval other                                      = return $ Success other

-- Evaluating a LipsVal (printing any error messages)
eval :: LipsVal -> ProgramState LipsVal
eval lval = do
  errval <- safeEval lval
  
  case errval of
    Success val -> return val
    Error   et  -> (liftIO $ putStrLn $ displayErrorType et) >> return lNull
