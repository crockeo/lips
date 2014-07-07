module Language.Lips.Evaluator where

--------------------
-- Global Imports --
import Control.Monad.State (liftIO)
import Control.Monad

-------------------
-- Local Imports --
import Language.Lips.LanguageDef
import Language.Lips.State

----------
-- Code --

-- Lifting an error
liftError :: Error LipsVal -> LipsVal
liftError (Success a)  = a
liftError (Error   et) = LList [LAtom "println", LString $ displayErrorType et]

-- Binding a variable
eBind :: String -> LipsVal -> ProgramState (Error LipsVal)
eBind name val = do
  evaled <- eval val

  pushVariable name evaled
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

-- Performing IO on a LipsVal
performIO :: (LipsVal -> IO ()) -> LipsVal -> ProgramState (Error LipsVal)
performIO fn val = do
  errval <- safeEval val

  case errval of
    Success val -> (liftIO $ fn val) >> (return $ Success lNull)
    Error   et  -> return $ Error et

-- Printing a variable
ePrint :: LipsVal -> ProgramState (Error LipsVal)
ePrint = performIO $ putStr . show

-- Printing a variable with a newline
ePrintln :: LipsVal -> ProgramState (Error LipsVal)
ePrintln = performIO $ putStrLn . show

-- Re-evaluating an accessed variable
eVarLookup :: String -> [LipsVal] -> ProgramState (Error LipsVal)
eVarLookup name args = do
  hp <- hasPrimitive name
  if hp
    then mapM eval args >>= applyPrimitive name
    else do
      errval <- getVariable name

      case errval of
        Success val ->
          if null args
            then safeEval val
            else safeEval $ LList (val : args)
        Error   et  -> return $ Error et

-- Applying an LFunction to its arguments
eApply :: LipsVal -> [LipsVal] -> ProgramState (Error LipsVal)
eApply (LFunction names val) args
  | length names /= length args = return $ Error InvalidFunctionApplicationError
  | otherwise = do
    let zipped = zip names args

    forM_ zipped (\(name, arg) -> eBind name arg)
    v <- safeEval val
    forM_ zipped (\(name, arg) -> eDrop name)

    return v

-- Safely evaluating things
safeEval :: LipsVal -> ProgramState (Error LipsVal)
safeEval (LList [LAtom "bind"      , LAtom name, val]) = eBind name val
safeEval (LList [LAtom "drop"      , LAtom name     ]) = eDrop name
safeEval (LList [LAtom "print"     , val            ]) = ePrint val
safeEval (LList [LAtom "println"   , val            ]) = ePrintln val
safeEval (LList [LAtom "quote"     , val            ]) = return $ Success val
safeEval (LList (LAtom name        : args           )) = eVarLookup name args
safeEval (LAtom name                                 ) = eVarLookup name []
safeEval (LList (fn@(LFunction _ _): args           )) = eApply fn args
safeEval fn@(LFunction _ _                           ) = return $ Success fn
safeEval other                                         = return $ Success other

-- Evaluating a LipsVal (printing any error messages)
eval :: LipsVal -> ProgramState LipsVal
eval lval = do
  errval <- safeEval lval
  
  case errval of
    Success val -> return val
    Error   et  -> eval $ LList [LAtom "println", LString $ displayErrorType et]
