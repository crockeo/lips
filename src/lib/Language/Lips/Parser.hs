module Language.Lips.Parser where

--------------------
-- Global Imports --
import Text.Parsec.String
import Text.Parsec hiding (spaces)

import Control.Monad

import Data.Monoid

-------------------
-- Local Imports --
import Language.Lips.LanguageDef

----------
-- Code --

-- Matching a symbol
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- One or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- Parsing an LAtom
parseLAtom :: Parser LipsVal
parseLAtom = do
  first <-        choice [letter, symbol]
  rest  <- many $ choice [letter, symbol, digit]

  return $ case first : rest of
    "#t"  -> LBool True
    "#f"  -> LBool False
    other -> LAtom other

-- Parsing an LList
parseLList :: Parser LipsVal
parseLList = liftM LList $ between (char '(') (char ')') $ sepBy lipsParser spaces

-- Parsing an LDottedList
parseLDottedList :: Parser LipsVal
parseLDottedList =
  between (char '(') (char ')') $ do
    list <- lipsParser `endBy` spaces
    end  <- char '.' >> spaces >> lipsParser

    return $ LDottedList list end

-- Parsing an LNumber
parseLNumber :: Parser LipsVal
parseLNumber = do
  whole <- many1 digit
  decim <- option "0" $ (char '.' >> many1 digit)

  return $ LNumber $ (read $ mconcat [whole, ".", decim] :: Double)

-- Parsing an LString
parseLString :: Parser LipsVal
parseLString = liftM LString $ between (char '"') (char '"') (many1 $ noneOf "\"")

-- Single-quote syntax
parseQuoted :: Parser LipsVal
parseQuoted = do
  char '\''
  lips <- lipsParser
  return $ LList [LAtom "quote", lips]

-- A singular lips parser that applies 
lipsParser :: Parser LipsVal
lipsParser =
  choice $ map try parsers
  where parsers = [ parseLAtom
                  , parseLList
                  , parseLDottedList
                  , parseLNumber
                  , parseLString
                  , parseQuoted
                  ]

-- Parsing out a lips value
parseLips :: String -> LipsVal
parseLips input =
  case parse lipsParser "parseLips" input of
    Left  err -> LAtom $ show err
    Right val -> val
