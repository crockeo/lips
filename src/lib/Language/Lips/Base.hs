module Language.Lips.Base where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map

import Data.Monoid

-------------------
-- Local Imports --
import Language.Lips.LanguageDef
import Language.Lips.Error
import Language.Lips.State

----------
-- Code --

-- Creating a binary operator
makeBinaryOperator :: (Double -> Double -> Double) -> Function
makeBinaryOperator op =
  \l -> Success $ LNumber $ foldl1 op $ map toNum l
  where toNum :: LipsVal -> Double
        toNum (LNumber n) = n
        toNum other       = 0.0

-- A list of pre-implemented functions
primitives :: Map.Map String Function
primitives =
  Map.fromList [ ("+", makeBinaryOperator (+))
               , ("-", makeBinaryOperator (-))
               , ("*", makeBinaryOperator (*))
               , ("/", makeBinaryOperator (/))
               , ("pi", \l -> Success $ LNumber 3.14159265359)
               ]

-- Getting a primitive function
getPrimitive :: String -> Function
getPrimitive name =
  case Map.lookup name primitives of
    Nothing  -> \l -> Error VariableNotDefinedError
    Just val -> val
