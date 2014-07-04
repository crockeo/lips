module Language.Lips.Error where

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
