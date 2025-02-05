module IntegrationSpec where

import HelperLib (testRun)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "running example programs" $ do
    it "example program 'fak' evaluates to 720" $ testRun "f x = if x < 0 then 1 else x * f(x - 1); main = f 6;" `shouldBe` Right "82"

    it "example program 'let' evaluates to 5" $ testRun "f x = let y = x ; x = 5 in y; main = (f 1);" `shouldBe` Right "5"

    it "example program 'quadrat' evaluates to 81" $ testRun "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;" `shouldBe` Right "81"

    it "example program 'second' evaluates to 2" $ testRun "main = second 1 2; second x y = y;" `shouldBe` Right "2"