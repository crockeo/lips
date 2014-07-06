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

-- Parsing a token
parseToken :: Parser String
parseToken = do
  first <-        choice [letter, symbol]
  rest  <- many $ choice [letter, symbol, digit]

  return $ first : rest

-- Parsing an LAtom
parseLAtom :: Parser LipsVal
parseLAtom = do
  str <- parseToken
  case str of
    "#t"  -> return $ LBool True
    "#f"  -> return $ LBool False
    other -> return $ LAtom other

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

-- Parsing an LFunction
parseLFunction :: Parser LipsVal
parseLFunction =
  between (char '(') (char ')') $ do
    try $ string "lambda"
    spaces
    args <- between (char '(') (char ')') $ sepBy parseToken spaces
    spaces
    val  <- lipsParser

    return $ LFunction args val

-- Single-quote syntax
parseQuoted :: Parser LipsVal
parseQuoted = do
  char '\''
  lips <- lipsParser
  return $ LList [LAtom "quote", lips]

-- A singular lips parser that applies 
lipsParser :: Parser LipsVal
lipsParser = do
  optional spaces
  choice $ map try parsers
  where parsers = [ parseLAtom
                  , parseLFunction
                  , parseLList
                  , parseLDottedList
                  , parseLNumber
                  , parseLString
                  , parseQuoted
                  ]

-- Parsing out lips values
lips :: String -> LipsVal
lips input =
  case parse lipsParser "lips" input of
    Left  err -> LAtom $ show err
    Right val -> val

manyLips :: String -> [LipsVal]
manyLips input =
  case parse (many1 lipsParser) "manyLips" input of
    Left  err -> [LAtom $ show err]
    Right val -> val
