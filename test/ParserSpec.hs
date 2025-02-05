module ParserSpec where

import Data.Either (isRight)
import Parser (ParseResult, unsafeParse)
import SyntaxTree (Expression (..), LocalDefinition (LocalDefinition), Program)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "parser tests" $ do
  describe "atomic structures" $ do
    it "can parse simple let expression" $ unsafeParse "let x = 1 in 1" `shouldBe` Right (Let [LocalDefinition "x" (Number 1)] (Number 1))

    it "can parse simple if-then-else expression" $ unsafeParse "if true then 1 else 1" `shouldBe` Right (IfThenElse (Boolean True) (Number 1) (Number 1))

    it "can parse simple disjunction expression" $ unsafeParse "false | false" `shouldBe` Right (Disjunction (Boolean False) (Boolean False))

    it "can parse simple conjunction expression" $ unsafeParse "false & false" `shouldBe` Right (Conjunction (Boolean False) (Boolean False))

    it "can parse simple negation expression" $ unsafeParse "not false" `shouldBe` Right (LogicalNegation (Boolean False))

    it "can parse simple smaller expression" $ unsafeParse "1 < 2" `shouldBe` Right (Smaller (Number 1) (Number 2))

    it "can parse simple equality expression" $ unsafeParse "false == false" `shouldBe` Right (Equality (Boolean False) (Boolean False))

    it "can parse simple minus expression" $ unsafeParse "- 1" `shouldBe` Right (Minus (Number 1))

    it "can parse simple difference expression" $ unsafeParse "1 - 1" `shouldBe` Right (Difference (Number 1) (Number 1))

    it "can parse simple sum expression" $ unsafeParse "1 + 1" `shouldBe` Right (Sum (Number 1) (Number 1))

    it "can parse simple quotient expression" $ unsafeParse "1 / 1" `shouldBe` Right (Quotient (Number 1) (Number 1))

    it "can parse simple product expression" $ unsafeParse "1 * 1" `shouldBe` Right (Product (Number 1) (Number 1))

    it "can parse simple application expression" $ unsafeParse "f 1" `shouldBe` Right (Application (Variable "f") (Number 1))

    it "can parse atomic variable expression" $ unsafeParse "var" `shouldBe` Right (Variable "var")

    it "can parse atomic number expression" $ unsafeParse "1" `shouldBe` Right (Number 1)

    it "can parse atomic boolean expression" $ unsafeParse "true" `shouldBe` Right (Boolean True)

  describe "operator precedences" $ do
    it "'&' binds more strongly than '|'" $ unsafeParse "0 & 0 | 0" `shouldBe` Right (Disjunction (Conjunction (Number 0) (Number 0)) (Number 0))

    it "'not' binds more strongly than '&'" $ unsafeParse "not 0 & 0" `shouldBe` Right (Conjunction (LogicalNegation (Number 0)) (Number 0))

    it "'<' binds more strongly than 'not'" $ unsafeParse "not 0 < 0" `shouldBe` Right (LogicalNegation (Smaller (Number 0) (Number 0)))

    it "'==' binds more strongly than 'not'" $ unsafeParse "not 0 == 0" `shouldBe` Right (LogicalNegation $ Equality (Number 0) (Number 0))

    it "unary '-' binds more strongly than '<'" $ unsafeParse "- 0 < 0" `shouldBe` Right (Smaller (Minus $ Number 0) (Number 0))

    it "unary '-' binds more strongly than '=='" $ unsafeParse "- 0 == 0" `shouldBe` Right (Equality (Minus $ Number 0) (Number 0))

    it "binary '-' binds more strongly than unary '-'" $ unsafeParse "- 0 - 0" `shouldBe` Right (Minus $ Difference (Number 0) (Number 0))

    it "'+' binds more strongly than binary '-'" $ unsafeParse "0 - 0 + 0" `shouldBe` Right (Difference (Number 0) (Sum (Number 0) (Number 0)))

    it "'/' binds more strongly than '+'" $ unsafeParse "0 / 0 + 0" `shouldBe` Right (Sum (Quotient (Number 0) (Number 0)) (Number 0))

    it "'*' binds more strongly than '/'" $ unsafeParse "0 * 0 / 0" `shouldBe` Right (Quotient (Product (Number 0) (Number 0)) (Number 0))

    it "application binds more strongly than '/'" $ unsafeParse "f 0 / 0" `shouldBe` Right (Quotient (Application (Variable "f") (Number 0)) (Number 0))

  describe "example prorams" $ do
    it "can parse boolfak example program" $ (unsafeParse "bool x = x == true | x == false; f x = if bool x | x < 0 then 1 else x * f (x - 1); main = f 6;" :: ParseResult Program) `shouldSatisfy` isRight

    it "can parse fak example program" $ (unsafeParse "f x = if x < 0 then 1 else x * f(x - 1); main = f 6;" :: ParseResult Program) `shouldSatisfy` isRight

    it "can parse let example program" $ (unsafeParse "f x = let y = x ; x = 5 in y; main = (f 1);" :: ParseResult Program) `shouldSatisfy` isRight

    it "can parse quadrat example program" $ (unsafeParse "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;" :: ParseResult Program) `shouldSatisfy` isRight

    it "can parse second example program" $ (unsafeParse "main = second 1 2; second x y = y;" :: ParseResult Program) `shouldSatisfy` isRight

  describe "some other interesting properties" $ describe "brackets" $ do
    it "brackets work in the simple expression '0 == (not 0)', that otherwise doesn't parse" $ unsafeParse "0 == (not 0)" `shouldBe` Right (Equality (Number 0) (LogicalNegation $ Number 0))

    it "application binds in correct order" $ unsafeParse "f a b c" `shouldBe` Right (Application (Application (Application (Variable "f") (Variable "a")) (Variable "b")) (Variable "c"))

    it "multiple non-bracketed operators expressions parse left-associatively" $ unsafeParse "a + b + c + d" `shouldBe` Right (Sum (Variable "a") (Sum (Variable "b") (Sum (Variable "c") (Variable "d"))))