module Language.Lips.Language where

--------------------
-- Global Imports --
import Data.Monoid

----------
-- Code --
data LipsVal = LAtom       String
             | LList       [LipsVal]
             | LDottedList [LipsVal] LipsVal
             | LNumber     Double
             | LString     String
             | LBool       Bool

instance Show LipsVal where
  show (LAtom       name  ) = name
  show (LList       list  ) = mconcat ["(", foldl1 (\a b -> a ++ " " ++ b) $ map (show) list, ")"]
  show (LDottedList list v) = mconcat ["(", show $ LList list, " . ", show v, ")"]
  show (LNumber     number) = show number
  show (LString     string) = show string
  show (LBool       True  ) = "#t"
  show (LBool       False ) = "#f"
