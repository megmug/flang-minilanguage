module IntegrationSpec where

import Data.Either (isLeft)
import HelperLib (testRun)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

ackermannDef :: String
ackermannDef = "ack n m = if n == 0 then m + 1 else if m == 0 then ack (n - 1) 1 else ack (n - 1) (ack n (m - 1));"

facultyDef :: String
facultyDef = "fak n = if n < 2 then 1 else n * fak (n - 1);"

spec :: Spec
spec = do
  describe "running example programs from the script" $ do
    it "example program 'fak' evaluates to 720" $ testRun "f x = if x < 1 then 1 else x * f(x - 1); main = f 6;" `shouldBe` Right "720"
    it "example program 'let' evaluates to 5" $ testRun "f x = let y = x ; x = 5 in y; main = (f 1);" `shouldBe` Right "5"
    it "example program 'quadrat' evaluates to 81" $ testRun "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;" `shouldBe` Right "81"
    it "example program 'second' evaluates to 2" $ testRun "main = second 1 2; second x y = y;" `shouldBe` Right "2"

  describe "recursion" $ do
    it "'ackermann n m' with n = 0, m = 0 evaluates to 1" $ testRun ("main = ack 0 0;" ++ ackermannDef) `shouldBe` Right "1"
    it "'ackermann n m' with n = 1, m = 1 evaluates to 3" $ testRun ("main = ack 1 1;" ++ ackermannDef) `shouldBe` Right "3"
    it "'ackermann n m' with n = 2, m = 2 evaluates to 7" $ testRun ("main = ack 2 2;" ++ ackermannDef) `shouldBe` Right "7"
    it "'ackermann n m' with n = 3, m = 3 evaluates to 61" $ testRun ("main = ack 3 3;" ++ ackermannDef) `shouldBe` Right "61"
    it "faculty 1 evaluates to 1" $ testRun ("main = fak 1;" ++ facultyDef) `shouldBe` Right "1"
    it "faculty 2 evaluates to 2" $ testRun ("main = fak 2;" ++ facultyDef) `shouldBe` Right "2"
    it "faculty 3 evaluates to 6" $ testRun ("main = fak 3;" ++ facultyDef) `shouldBe` Right "6"
    it "faculty 4 evaluates to 24" $ testRun ("main = fak 4;" ++ facultyDef) `shouldBe` Right "24"
    it "faculty 6 evaluates to 720" $ testRun ("main = fak 6;" ++ facultyDef) `shouldBe` Right "720"

  describe "extremely simple base cases" $ do
    describe "arithmetical and logical connectives" $ do
      it "'main = 5 * 5;' evaluates to 25" $ testRun "main = 5 * 5;" `shouldBe` Right "25"
      it "'main = 5 * false;' doesn't evaluate" $ testRun "main = 5 * false;" `shouldSatisfy` isLeft
      it "'main = 5 / 5;' evaluates to 1" $ testRun "main = 5 / 5;" `shouldBe` Right "1"
      it "'main = 5 / 6;' evaluates to 0" $ testRun "main = 5 / 6;" `shouldBe` Right "0"
      it "'main = 10 / 3;' evaluates to 3" $ testRun "main = 10 / 3;" `shouldBe` Right "3"
      it "'main = 10 / false;' doesn't evaluate" $ testRun "main = 10 / false;" `shouldSatisfy` isLeft
      it "'main = 6 + 3;' evaluates to 9" $ testRun "main = 6 + 3;" `shouldBe` Right "9"
      it "'main = true + 3;' doesn't evaluate" $ testRun "main = true + 3;" `shouldSatisfy` isLeft
      it "'main = 6 - 3;' evaluates to 3" $ testRun "main = 6 - 3;" `shouldBe` Right "3"
      it "'main = 3 - 6;' evaluates to -3" $ testRun "main = 3 - 6;" `shouldBe` Right "-3"
      it "'main = 3 - true;' doesn't evaluate" $ testRun "main = 3 - true;" `shouldSatisfy` isLeft
      it "'main = - (-6);' evaluates to 6" $ testRun "main = - (-6);" `shouldBe` Right "6"
      it "'main = - false;' doesn't evaluate" $ testRun "main = - false;" `shouldSatisfy` isLeft
      it "'main = 3 == 3;' evaluates to true" $ testRun "main = 3 == 3;" `shouldBe` Right "true"
      it "'main = 3 == 5;' evaluates to false" $ testRun "main = 3 == 5;" `shouldBe` Right "false"
      it "'main = 3 == true;' doesn't evaluate" $ testRun "main = 3 == true;" `shouldSatisfy` isLeft
      it "'main = 3 < 3;' evaluates to false" $ testRun "main = 3 < 3;" `shouldBe` Right "false"
      it "'main = 3 < 4;' evaluates to true" $ testRun "main = 3 < 4;" `shouldBe` Right "true"
      it "'main = false < false;' doesn't evaluate" $ testRun "main = false < false;" `shouldSatisfy` isLeft
      it "'main = not true;' evaluates to false" $ testRun "main = not true;" `shouldBe` Right "false"
      it "'main = not (not true);' evaluates to false" $ testRun "main = not (not true);" `shouldBe` Right "true"
      it "'main = not 3;' doesn't evaluate" $ testRun "main = not 3;" `shouldSatisfy` isLeft
      it "'main = true & false;' evaluates to false" $ testRun "main = true & false;" `shouldBe` Right "false"
      it "'main = true & true;' evaluates to true" $ testRun "main = true & true;" `shouldBe` Right "true"
      it "'main = true & 3;' doesn't evaluate" $ testRun "main = true & 3;" `shouldSatisfy` isLeft
      it "'main = true | false;' evaluates to true" $ testRun "main = true | false;" `shouldBe` Right "true"
      it "'main = false | false;' evaluates to false" $ testRun "main = false | false;" `shouldBe` Right "false"
      it "'main = true | 3;' doesn't evaluate" $ testRun "main = true | 3;" `shouldSatisfy` isLeft

    describe "atomic expressions (except variables)" $ do
      it "'main = 5;' evaluates to 5" $ testRun "main = 5;" `shouldBe` Right "5"
      it "'main = false;' evaluates to false" $ testRun "main = false;" `shouldBe` Right "false"

    describe "function application" $ do
      it "'main = f 5; f x = x;' evaluates to 5" $ testRun "main = f 5; f x = x;" `shouldBe` Right "5"

    describe "let" $ do
      it "'main = let x = 1 in x;' evaluates to 1" $ testRun "main = let x = 1 in x;" `shouldBe` Right "1"
      it "'main = let x = y; y = 1 in x;' evaluates to 1" $ testRun "main = let x = y; y = 1 in x;" `shouldBe` Right "1"

    describe "if-then-else" $ do
      it "'main = if true then 1 else 0;' evaluates to 1" $ testRun "main = if true then 1 else 0;" `shouldBe` Right "1"
      it "'main = if false then 1 else 0;' evaluates to 0" $ testRun "main = if false then 1 else 0;" `shouldBe` Right "0"
      it "'main = if ( if false then true else false ) then 1 else 0;' evaluates to 0" $ testRun "main = if ( if false then true else false ) then 1 else 0;" `shouldBe` Right "0"