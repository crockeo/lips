module Language.Lips.Evaluator where

-------------------
-- Local Imports --
import Language.Lips.LanguageDef

----------
-- Code --

-- Evaluating a LipsVal operation
eval :: LipsVal -> LipsVal
eval (LList [LAtom "quote", val]) = val
eval other                        = other