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

-- A function type synonym
type Function = [LipsVal] -> LipsVal

-- Program data type
data Program = Program { variables :: Map.Map String Function }

-- A program state type synonym
type ProgramState a = StateT Program IO a

-- Pushing a variable
pushVariable :: String -> Function -> ProgramState ()
pushVariable key fn =
  state $ \program -> ((), program { variables = Map.insert key fn $ variables program })

-- Dropping a variable
dropVariable :: String -> ProgramState ()
dropVariable key =
  state $ \program -> ((), program { variables = Map.delete key $ variables program })

-- Getting a variable
getVariable :: String -> ProgramState Function
getVariable key =
  state $ \program ->
    (case Map.lookup key $ variables program of
       Nothing  -> \l -> LString $ mconcat ["Error: Could not find definition for variable name '", key, "'"]
       Just val -> val, program)
