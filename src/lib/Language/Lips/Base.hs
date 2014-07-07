module Language.Lips.Base where

--------------------
-- Global Imports --
import qualified Data.Map.Strict as Map

import Control.Monad

-------------------
-- Local Imports --
import Language.Lips.LanguageDef

----------
-- Code --

-- A function type
type Primitive = [LipsVal] -> Error LipsVal

-- Making a binary operator
makeBinaryOperator :: (Double -> Double -> Double) -> Primitive
makeBinaryOperator op =
  \l -> liftM (LNumber . foldl1 op) $ mapM toNum l
  where toNum :: LipsVal -> Error Double
        toNum (LNumber n) = Success n
        toNum other       = Error InvalidTypeError

-- The base primitives
basePrimitives :: Map.Map String Primitive
basePrimitives = Map.fromList [ ("+", makeBinaryOperator (+))
                              , ("-", makeBinaryOperator (-))
                              , ("*", makeBinaryOperator (*))
                              , ("/", makeBinaryOperator (/))
                              ]
