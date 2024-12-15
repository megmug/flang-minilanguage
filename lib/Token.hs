module Token where

import Text.Parsec (SourcePos)

-- this is the type that the parser requires - a stream of tokens paired with their respective source code positions
type TokenPos = (Token, SourcePos)

-- these are the (abstract) tokens that can occur in an F program
data Token
  = Semicolon
  | (:=)
  | Let
  | In
  | If
  | Then
  | Else
  | (:&)
  | (:|)
  | (:==)
  | (:<)
  | (:+)
  | (:-)
  | (:*)
  | (:/)
  | Not
  | OpenRoundBracket
  | CloseRoundBracket
  | Name String
  | Integer Integer
  | Boolean Bool
  deriving (Eq, Show)