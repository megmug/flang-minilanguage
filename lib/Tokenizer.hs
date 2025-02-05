module Tokenizer where

import Text.Parsec
  ( ParseError,
    Parsec,
    choice,
    digit,
    endOfLine,
    eof,
    getPosition,
    many,
    many1,
    parse,
    satisfy,
    space,
    string,
    tab,
    try,
  )
import Token (Token (..), TokenPos)

-- we have tokenizers for single tokens
type UnitTokenizer = Parsec String () TokenPos

-- a tokenizer for tokenizing a whole stream
type Tokenizer = Parsec String () [TokenPos]

-- tokenizers that ignore certain chars
type Ignorer = Parsec String () Char

getUnitTokenizer :: (String, Token) -> UnitTokenizer
getUnitTokenizer (s, t) = ((,) t <$> getPosition) <* string s

getUnitTokenizers :: [(String, Token)] -> [UnitTokenizer]
getUnitTokenizers = map getUnitTokenizer

-- here we define the mapping from our basic string tokens to their atomic abstract token counterparts
reservedSymbolsAndWords :: [UnitTokenizer]
reservedSymbolsAndWords =
  getUnitTokenizers
    [ (";", Semicolon),
      ("==", (:==)), -- this needs to be placed before "=" to ensure that it gets tried first, otherwise it will never match
      ("=", (:=)),
      ("let", Let),
      ("in", In),
      ("if", If),
      ("then", Then),
      ("else", Else),
      ("&", (:&)),
      ("|", (:|)),
      ("<", (:<)),
      ("+", (:+)),
      ("-", (:-)),
      ("*", (:*)),
      ("/", (:/)),
      ("not", Not),
      ("(", OpenRoundBracket),
      (")", CloseRoundBracket),
      ("true", Boolean True),
      ("false", Boolean False)
    ]

lowerchar :: [Char]
lowerchar = ['a' .. 'z']

name :: UnitTokenizer
name = do
  p <- getPosition
  n <- many1 $ satisfy (`elem` lowerchar)
  return (Name n, p)

integer :: UnitTokenizer
integer = do
  p <- getPosition
  ds <- many1 digit
  return (Integer $ read ds, p)

anySingleToken :: UnitTokenizer
anySingleToken = choice $ try <$> reservedSymbolsAndWords ++ [name, integer]

anyIgnored :: Ignorer
anyIgnored = choice [space, endOfLine, tab]

-- this is our program tokenizer
tokenizer :: Tokenizer
tokenizer = do
  ts <- many $ try $ many anyIgnored *> anySingleToken
  _ <- many anyIgnored
  eof
  return ts

tokenize :: String -> Either ParseError [TokenPos]
tokenize = Text.Parsec.parse tokenizer "input"