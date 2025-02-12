module CodeGeneratorSpec where

import Control.Exception (evaluate)
import Data.Either (isLeft, isRight)
import HelperLib (testGenerate)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "example programs compilation" $ do
    it "example program 'boolfak' DOES NOT compile due to typing error" $ do
      testGenerate "bool x = x == true | x == false; f x = if bool x | x < 0 then 1 else x * f (x - 1); main = f 6;" `shouldSatisfy` isLeft

    it "example program 'fak' DOES compile" $ do
      testGenerate "f x = if x < 0 then 1 else x * f(x - 1); main = f 6;" `shouldSatisfy` isRight

    it "example program 'let' DOES compile" $ do
      testGenerate "f x = let y = x ; x = 5 in y; main = (f 1);" `shouldSatisfy` isRight

    it "example program 'quadrat' DOES compile" $ do
      testGenerate "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;" `shouldSatisfy` isRight

    it "example program 'second' DOES compile" $ do
      testGenerate "main = second 1 2; second x y = y;" `shouldSatisfy` isRight

  describe "catching common errors" $ do
    it "'f = 1;' DOES NOT compile (no main definition)" $ testGenerate "f = 1;" `shouldSatisfy` isLeft
    it "'main = x;' DOES NOT compile (undefined variable/function)" $ testGenerate "main = x;" `shouldSatisfy` isLeft
    it "'main = f; f = 1; f = 2;' DOES NOT compile (conflicting function definitions)" $ testGenerate "main = f; f = 1; f = 2;" `shouldSatisfy` isLeft
    it "'main = f 0 0; f x x = x;' DOES NOT compile (conflicting parameter bindings)" $ testGenerate "main = f 0 0; f x x = x;" `shouldSatisfy` isLeft
    it "'main = let x = 1; x = 5 in x;' DOES NOT compile (conflicting let bindings)" $ testGenerate "main = let x = 1; x = 5 in x;" `shouldSatisfy` isLeft
    it "'main = let x = y; y = x in x;' DOES NOT compile (recursive let bindings)" $ do
      mRes <- timeout (5 * 1000000) (evaluate (testGenerate "main = let x = y; y = x in x;"))
      case mRes of
        Nothing -> expectationFailure "Test timed out after 5s"
        Just res -> res `shouldSatisfy` isLeft
