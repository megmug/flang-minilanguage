{-# LANGUAGE DataKinds #-}

module ParserSpec where

import Data.Either (isRight)
import Parser (ParseResult)
import SyntaxTree (Expression (..), LocalDefinition (LocalDefinition), Program, Stage (Raw))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestLib (testParse)

spec :: Spec
spec = describe "parser tests" $ do
  describe "atomic structures" $ do
    it "can parse simple let expression" $ testParse "let x = 1 in 1" `shouldBe` Right (Let [LocalDefinition "x" (Number 1)] (Number 1))

    it "can parse simple if-then-else expression" $ (testParse "if true then 1 else 1" :: ParseResult (Expression Raw)) `shouldBe` Right (IfThenElse (Boolean True) (Number 1) (Number 1))

    it "can parse simple disjunction expression" $ (testParse "false | false" :: ParseResult (Expression Raw)) `shouldBe` Right (Disjunction (Boolean False) (Boolean False))

    it "can parse simple conjunction expression" $ (testParse "false & false" :: ParseResult (Expression Raw)) `shouldBe` Right (Conjunction (Boolean False) (Boolean False))

    it "can parse simple negation expression" $ (testParse "not false" :: ParseResult (Expression Raw)) `shouldBe` Right (LogicalNegation (Boolean False))

    it "can parse simple smaller expression" $ (testParse "1 < 2" :: ParseResult (Expression Raw)) `shouldBe` Right (Smaller (Number 1) (Number 2))

    it "can parse simple equality expression" $ (testParse "false == false" :: ParseResult (Expression Raw)) `shouldBe` Right (Equality (Boolean False) (Boolean False))

    it "can parse simple minus expression" $ (testParse "- 1" :: ParseResult (Expression Raw)) `shouldBe` Right (Minus (Number 1))

    it "can parse simple difference expression" $ (testParse "1 - 1" :: ParseResult (Expression Raw)) `shouldBe` Right (Difference (Number 1) (Number 1))

    it "can parse simple sum expression" $ (testParse "1 + 1" :: ParseResult (Expression Raw)) `shouldBe` Right (Sum (Number 1) (Number 1))

    it "can parse simple quotient expression" $ (testParse "1 / 1" :: ParseResult (Expression Raw)) `shouldBe` Right (Quotient (Number 1) (Number 1))

    it "can parse simple product expression" $ (testParse "1 * 1" :: ParseResult (Expression Raw)) `shouldBe` Right (Product (Number 1) (Number 1))

    it "can parse simple application expression" $ (testParse "f 1" :: ParseResult (Expression Raw)) `shouldBe` Right (Application (Variable "f") (Number 1))

    it "can parse atomic variable expression" $ (testParse "var" :: ParseResult (Expression Raw)) `shouldBe` Right (Variable "var")

    it "can parse atomic number expression" $ (testParse "1" :: ParseResult (Expression Raw)) `shouldBe` Right (Number 1)

    it "can parse atomic boolean expression" $ (testParse "true" :: ParseResult (Expression Raw)) `shouldBe` Right (Boolean True)

  describe "operator precedences" $ do
    it "'&' binds more strongly than '|'" $ (testParse "0 & 0 | 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Disjunction (Conjunction (Number 0) (Number 0)) (Number 0))

    it "'not' binds more strongly than '&'" $ (testParse "not 0 & 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Conjunction (LogicalNegation (Number 0)) (Number 0))

    it "'<' binds more strongly than 'not'" $ (testParse "not 0 < 0" :: ParseResult (Expression Raw)) `shouldBe` Right (LogicalNegation (Smaller (Number 0) (Number 0)))

    it "'==' binds more strongly than 'not'" $ (testParse "not 0 == 0" :: ParseResult (Expression Raw)) `shouldBe` Right (LogicalNegation $ Equality (Number 0) (Number 0))

    it "unary '-' binds more strongly than '<'" $ (testParse "- 0 < 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Smaller (Minus $ Number 0) (Number 0))

    it "unary '-' binds more strongly than '=='" $ (testParse "- 0 == 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Equality (Minus $ Number 0) (Number 0))

    it "binary '-' binds more strongly than unary '-'" $ (testParse "- 0 - 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Minus $ Difference (Number 0) (Number 0))

    it "'+' binds more strongly than binary '-'" $ (testParse "0 - 0 + 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Difference (Number 0) (Sum (Number 0) (Number 0)))

    it "'/' binds more strongly than '+'" $ (testParse "0 / 0 + 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Sum (Quotient (Number 0) (Number 0)) (Number 0))

    it "'*' binds more strongly than '/'" $ (testParse "0 * 0 / 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Quotient (Product (Number 0) (Number 0)) (Number 0))

    it "application binds more strongly than '/'" $ (testParse "f 0 / 0" :: ParseResult (Expression Raw)) `shouldBe` Right (Quotient (Application (Variable "f") (Number 0)) (Number 0))

  describe "example prorams" $ do
    it "can parse boolfak example program" $ (testParse "bool x = x == true | x == false; f x = if bool x | x < 0 then 1 else x * f (x - 1); main = f 6;" :: ParseResult (Program Raw)) `shouldSatisfy` isRight

    it "can parse fak example program" $ (testParse "f x = if x < 0 then 1 else x * f(x - 1); main = f 6;" :: ParseResult (Program Raw)) `shouldSatisfy` isRight

    it "can parse let example program" $ (testParse "f x = let y = x ; x = 5 in y; main = (f 1);" :: ParseResult (Program Raw)) `shouldSatisfy` isRight

    it "can parse quadrat example program" $ (testParse "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;" :: ParseResult (Program Raw)) `shouldSatisfy` isRight

    it "can parse second example program" $ (testParse "main = second 1 2; second x y = y;" :: ParseResult (Program Raw)) `shouldSatisfy` isRight

  describe "some other interesting properties" $ describe "brackets" $ do
    it "brackets work in the simple expression '0 == (not 0)', that otherwise doesn't parse" $ (testParse "0 == (not 0)" :: ParseResult (Expression Raw)) `shouldBe` Right (Equality (Number 0) (LogicalNegation $ Number 0))

    it "application binds in correct order" $ (testParse "f a b c" :: ParseResult (Expression Raw)) `shouldBe` Right (Application (Application (Application (Variable "f") (Variable "a")) (Variable "b")) (Variable "c"))

    it "multiple non-bracketed operators expressions parse left-associatively" $ (testParse "a + b + c + d" :: ParseResult (Expression Raw)) `shouldBe` Right (Sum (Variable "a") (Sum (Variable "b") (Sum (Variable "c") (Variable "d"))))