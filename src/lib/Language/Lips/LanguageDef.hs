module Language.Lips.LanguageDef where

--------------------
-- Global Imports --
import Control.Applicative
import Data.Monoid

----------
-- Code --

-- Error types
data ErrorType = VariableNotDefinedError
  deriving (Eq, Show, Read)

-- Pretilly displaying an ErrorType
displayErrorType :: ErrorType -> String
displayErrorType t = "Error thrown: " ++ show t

-- The actual error monad
data Error a = Success a | Error ErrorType

-- Showing an error
instance (Show a) => Show (Error a) where
  show (Success a) = "Success " ++ show a
  show (Error   t) = "Error " ++ show t

-- A functor instance
instance Functor Error where
  fmap fn (Success a) = Success $ fn a
  fmap fn (Error   t) = Error t

-- A monad instance
instance Monad Error where
  return a = Success a
  err >>= fn =
    case err of
      Success a  -> fn a
      Error   et -> Error et

-- An applicative instance
instance Applicative Error where
  pure a = return a
  efn <*> err = do
    fn  <- efn
    val <- err

    return $ fn val

-- LipsVal definition
data LipsVal = LAtom       String
             | LList       [LipsVal]
             | LDottedList [LipsVal] LipsVal
             | LNumber     Double
             | LString     String
             | LBool       Bool
             | LFunction   [LipsVal] -> Error LipsVal

-- Show instance for LipsVal
instance Show LipsVal where
  show (LAtom       name  ) = name
  show (LList       list  ) = mconcat ["(", unwords $ map (show) list, ")"]
  show (LDottedList list v) = mconcat ["(", show $ LList list, " . ", show v, ")"]
  show (LNumber     number) = show number
  show (LString     string) = show string
  show (LBool       True  ) = "#t"
  show (LBool       False ) = "#f"

-- A null definition
lNull :: LipsVal
lNull = LList []
