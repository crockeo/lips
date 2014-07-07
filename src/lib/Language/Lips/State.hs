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
import Language.Lips.Base

----------
-- Code --

-- Program data type
data Program = Program { primitives :: Map.Map String Primitive
                       , variables  :: Map.Map String LipsVal
                       }

-- A program state type synonym
type ProgramState a = StateT Program IO a

-- Getting a primitive safely
getPrimitiveSafe :: String -> ProgramState (Maybe Primitive)
getPrimitiveSafe key =
  state $ \program ->
    (Map.lookup key $ primitives program, program)

-- Checking if a primtive exists
hasPrimitive :: String -> ProgramState Bool
hasPrimitive key = do
  mp <- getPrimitiveSafe key
  return $ case mp of
    Just _  -> True
    Nothing -> False

-- Applying a list of LipsVals to a primitive
applyPrimitive :: String -> [LipsVal] -> ProgramState (Error LipsVal)
applyPrimitive key vals = do
  mp <- getPrimitiveSafe key
  return $ case mp of
    Just p  -> p vals
    Nothing ->  Error VariableNotDefinedError

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
    Just _  -> True
    Nothing -> False

-- Getting a variable unsafely
getVariable :: String -> ProgramState (Error LipsVal)
getVariable key = do
  mvs <- getVariableSafe key
  return $ case mvs of
    Just vs -> Success vs
    Nothing -> Error VariableNotDefinedError
