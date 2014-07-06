module Language.Lips.Error where

--------------------
-- Global Imports --
import Control.Applicative

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
