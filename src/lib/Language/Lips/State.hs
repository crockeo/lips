module Language.Lips.State where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad

import Data.Monoid

-------------------
-- Local Imports --
import Language.Lips.LanguageDef

----------
-- Code --

-- Program data type
data Program = Program { variables :: Map.Map String LipsVal }

-- A program state type synonym
type ProgramState a = StateT Program IO a

-- Pushing a variable
pushVariable :: String -> LipsVal -> ProgramState ()
pushVariable key fn =
  state $ \program -> ((), program { variables = Map.insert key fn $ variables program })

-- Dropping a variable
dropVariable :: String -> ProgramState ()
dropVariable key =
  state $ \program -> ((), program { variables = Map.delete key $ variables program })

-- Getting a variable safely
getVariableSafe :: String -> ProgramState (Maybe LipsVal)
getVariableSafe key =
  state $ \program ->
    (Map.lookup key $ variables program, program)

-- Checking if a variable exists
hasVariable :: String -> ProgramState Bool
hasVariable key = do
  mvs <- getVariableSafe key
  return $ case mvs of
    Just vs -> True
    Nothing -> False

-- Getting a variable unsafely
getVariable :: String -> ProgramState (Error LipsVal)
getVariable key = do
  mvs <- getVariableSafe key
  case mvs of
    Just vs -> return $ Success vs
    Nothing -> return $ Error VariableNotDefinedError
