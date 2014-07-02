module Language.Lips.Evaluator where

-------------------
-- Local Imports --
import Language.Lips.LanguageDef
import Language.Lips.Base

----------
-- Code --

-- Evaluating a LipsVal operation
eval :: LipsVal -> LipsVal
eval (LAtom func                ) = getPrimitive func []
eval (LList [LAtom "quote", val]) = val
eval (LList (LAtom func:args)   ) = getPrimitive func $ map eval args
eval other                        = other
