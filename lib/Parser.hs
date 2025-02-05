{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module Parser where

import Data.List.NonEmpty (NonEmpty ((:|)), init, last)
import SyntaxTree
  ( Definition (..),
    Expression
      ( Application,
        Boolean,
        Conjunction,
        Difference,
        Disjunction,
        Equality,
        IfThenElse,
        Let,
        LogicalNegation,
        Minus,
        Number,
        Product,
        Quotient,
        Smaller,
        Sum,
        Variable
      ),
    LocalDefinition (..),
    Program (..),
  )
import Text.Parsec.Combinator (many1, optionMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
  ( Parsec,
    many,
    parse,
    tokenPrim,
    (<|>),
  )
import Token (Token (..), TokenPos)

-- a parser generates some output by consuming a list of tokens + positions
type Parser a = Parsec [TokenPos] () a

type ParseResult a = Either ParseError a

-- all syntactic elements (SyntaxTree) are parseable
class Parseable a where
  parser :: Parser a

  parse :: [TokenPos] -> Either ParseError a
  parse = Text.Parsec.Prim.parse parser ""

instance Parseable Program where
  parser = program

program :: Parser Program
program = Program <$> many1 (definition <* accept Semicolon)

instance Parseable Definition where
  parser = definition

definition :: Parser Definition
definition = Definition <$> acceptName <*> many acceptName <*> (accept (:=) *> expression)

localDefinitions :: Parser [LocalDefinition]
localDefinitions = (:) <$> localDefinition <*> many (accept Semicolon *> localDefinition)

instance Parseable LocalDefinition where
  parser = localDefinition

localDefinition :: Parser LocalDefinition
localDefinition = LocalDefinition <$> acceptName <*> (accept (:=) *> expression)

instance Parseable Expression where
  parser = expression

expression :: Parser Expression
expression =
  disjunctionExpression
    <|> SyntaxTree.Let <$> (accept Token.Let *> localDefinitions) <*> (accept Token.In *> expression)
    <|> SyntaxTree.IfThenElse <$> (accept Token.If *> expression) <*> (accept Then *> expression) <*> (accept Else *> expression)

disjunctionExpression :: Parser Expression
disjunctionExpression = do
  e1 <- conjunctionExpression
  me2 <- optionMaybe (accept (Token.:|) *> disjunctionExpression)
  return $ case me2 of
    Nothing -> e1
    Just e2 -> Disjunction e1 e2

conjunctionExpression :: Parser Expression
conjunctionExpression = do
  e1 <- negationExpression
  me2 <- optionMaybe (accept (:&) *> conjunctionExpression)
  return $ case me2 of
    Nothing -> e1
    Just e2 -> Conjunction e1 e2

negationExpression :: Parser Expression
negationExpression = do
  mnot <- optionMaybe $ accept Not
  case mnot of
    Nothing -> smallerEqualsExpression
    Just _ -> LogicalNegation <$> smallerEqualsExpression

smallerEqualsExpression :: Parser Expression
smallerEqualsExpression = do
  e1 <- unaryMinusExpression
  msmaller <- optionMaybe $ accept (:<)
  case msmaller of
    Nothing -> do
      mequals <- optionMaybe $ accept (:==)
      case mequals of
        Nothing -> return e1
        Just _ -> Equality e1 <$> smallerEqualsExpression
    Just _ -> Smaller e1 <$> smallerEqualsExpression

unaryMinusExpression :: Parser Expression
unaryMinusExpression = do
  mminus <- optionMaybe $ accept (:-)
  case mminus of
    Nothing -> binaryMinusExpression
    Just _ -> Minus <$> binaryMinusExpression

binaryMinusExpression :: Parser Expression
binaryMinusExpression = do
  e1 <- additionExpression
  me2 <- optionMaybe (accept (:-) *> binaryMinusExpression)
  return $ case me2 of
    Nothing -> e1
    Just e2 -> Difference e1 e2

additionExpression :: Parser Expression
additionExpression = do
  e1 <- divisionExpression
  me2 <- optionMaybe (accept (:+) *> additionExpression)
  return $ case me2 of
    Nothing -> e1
    Just e2 -> Sum e1 e2

divisionExpression :: Parser Expression
divisionExpression = do
  e1 <- multiplicationExpression
  me2 <- optionMaybe (accept (:/) *> divisionExpression)
  return $ case me2 of
    Nothing -> e1
    Just e2 -> Quotient e1 e2

multiplicationExpression :: Parser Expression
multiplicationExpression = do
  e1 <- applicationExpression
  me2 <- optionMaybe (accept (:*) *> multiplicationExpression)
  return $ case me2 of
    Nothing -> e1
    Just e2 -> Product e1 e2

applicationExpression :: Parser Expression
applicationExpression = do
  first <- bracketedExpression
  rest <- many bracketedExpression
  let lastOf e es = Data.List.NonEmpty.last $ e Data.List.NonEmpty.:| es
      initOf e es = Data.List.NonEmpty.init $ e Data.List.NonEmpty.:| es
      makeApp [] e = e
      makeApp (e : es) e' = Application (makeApp (initOf e es) (lastOf e es)) e'
      appTree = makeApp (initOf first rest) (lastOf first rest)
  return appTree

bracketedExpression :: Parser Expression
bracketedExpression = atomicExpression <|> (accept OpenRoundBracket *> expression <* accept CloseRoundBracket)

atomicExpression :: Parser Expression
atomicExpression = (Variable <$> acceptName) <|> (Number <$> acceptInteger) <|> (SyntaxTree.Boolean <$> acceptBoolean)

{- Primitive helper functions
 - Most of these are only needed because we don't have a standard character parser, for which parsec is optimized
 - Rather, we use our own abstract token input stream type
 - So we need to reimplement things like advance, accept or satisfy
 -}

accept :: Token -> Parser Token
accept t = satisfy (== t)

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

acceptName :: Parser String
acceptName = tokenPrim show advance (\t -> case t of (Name n, _) -> Just n; _ -> Nothing)

acceptInteger :: Parser Integer
acceptInteger = tokenPrim show advance (\t -> case t of (Integer n, _) -> Just n; _ -> Nothing)

acceptBoolean :: Parser Bool
acceptBoolean = tokenPrim show advance (\t -> case t of (Token.Boolean b, _) -> Just b; _ -> Nothing)

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = tokenPrim show advance (\(t, _) -> if p t then Just t else Nothing)