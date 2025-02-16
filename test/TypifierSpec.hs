module TypifierSpec where

import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestLib (testTypify)
import Typifier (MonoType (..), TypeEquation (..), unify)

spec :: Spec
spec = do
  describe "untyped programs" $ do
    it "example program 'boolfak' DOES NOT typify" $ do
      testTypify "bool x = x == true | x == false; f x = if bool x | x < 0 then 1 else x * f (x - 1); main = f 6;" `shouldSatisfy` isLeft
  describe "simple unification problems" $ do
    it "[(a :->: b) :=: (Bool :->: Bool)] unifies to [a :=: FBool, b :=: FBool]" $ do
      unify [(TypeVariable "a" :->: TypeVariable "b") :=: (FBool :->: FBool)] `shouldBe` Just [TypeVariable "a" :=: FBool, TypeVariable "b" :=: FBool]
    it "[a :=: Bool, a :=: Integer] DOES NOT unify" $ do
      unify [TypeVariable "a" :=: FBool, TypeVariable "a" :=: FInteger] `shouldBe` Nothing