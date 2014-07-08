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

-- Getting an element from a list/string
getRaw :: [a] -> Int -> Error a
getRaw xs n
  | n >= length xs = Error InvalidArrayIndexError
  | n < 0          = Error InvalidArrayIndexError
  | otherwise      = Success $ xs !! n

get :: [LipsVal] -> Error LipsVal
get (LList   xs:LNumber index:[]) =                            getRaw xs $ floor index
get (LString xs:LNumber index:[]) = liftM (LString . return) $ getRaw xs $ floor index
get xs                            = Error InvalidFunctionApplicationError

-- Setting an element in a list/array
setRaw :: [a] -> Int -> a -> Error [a]
setRaw l n a
  | n >= length l = Error InvalidArrayIndexError
  | n < 0         = Error InvalidArrayIndexError
  | otherwise     = Success $ setRaw' l n a 0
  where setRaw' :: [a] -> Int -> a -> Int -> [a]
        setRaw' []     _ _ _ = []
        setRaw' (x:xs) n a c
          | n == c    = a : setRaw' xs n a (c + 1)
          | otherwise = x : setRaw' xs n a (c + 1)

set :: [LipsVal] -> Error LipsVal
set (LList   xs:LNumber index:x:[])              = liftM LList   $ setRaw xs (floor index) x
set (LString xs:LNumber index:LString (x:[]):[]) = liftM LString $ setRaw xs (floor index) x
set xs                                           = Error InvalidFunctionApplicationError

-- The id function
id :: [LipsVal] -> Error LipsVal
id (x:[]) = Success x
id other  = Error InvalidFunctionApplicationError

-- The base primitives
basePrimitives :: Map.Map String Primitive
basePrimitives = Map.fromList [ ("+", makeBinaryOperator (+))
                              , ("-", makeBinaryOperator (-))
                              , ("*", makeBinaryOperator (*))
                              , ("/", makeBinaryOperator (/))
                              , ("get", get)
                              , ("set", set)
                              , ("id" , id )
                              ]
