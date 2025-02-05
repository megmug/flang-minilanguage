module CodeGeneratorSpec where

import CodeGenerator (unsafeGenerate)
import Data.Either (isLeft, isRight)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = do
  describe "example programs evaluation" $ do
    it "example program 'boolfak' DOES NOT compile due to typing error" $ do
      unsafeGenerate "bool x = x == true | x == false; f x = if bool x | x < 0 then 1 else x * f (x - 1); main = f 6;" `shouldSatisfy` isLeft

    it "example program 'fak' DOES compile" $ do
      unsafeGenerate "f x = if x < 0 then 1 else x * f(x - 1); main = f 6;" `shouldSatisfy` isRight

    it "example program 'let' DOES compile" $ do
      unsafeGenerate "f x = let y = x ; x = 5 in y; main = (f 1);" `shouldSatisfy` isRight

    it "example program 'quadrat' DOES compile" $ do
      unsafeGenerate "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;" `shouldSatisfy` isRight

    it "example program 'second' DOES compile" $ do
      unsafeGenerate "main = second 1 2; second x y = y;" `shouldSatisfy` isRight
