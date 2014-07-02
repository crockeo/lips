module Language.Lips.State where

--------------------
-- Global Imports --
import qualified Data.Map.Strct as Map

import Control.Monad.State
import Control.Monad

-------------------
-- Local Imports --
import Language.Lips.LanguageDef

----------
-- Code --

-- A function type synonym
type Function = [LipsVal] -> LipsVal

-- Program data type
data Program = Program { variables :: Map String Function }

-- A program state type synonym
type ProgramState a = StateT Program IO a