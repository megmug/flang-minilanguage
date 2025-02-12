module TypifierSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Typifier (FType (..), TypeEquation (..), unify)

spec :: Spec
spec = do
  describe "simple unification problems" $ do
    it "[(a :->: b) :=: (Bool :->: Bool)] unifies to [a :=: FBool, b :=: FBool]" $ do
      unify [(TypeVariable "a" :->: TypeVariable "b") :=: (FBool :->: FBool)] `shouldBe` Just [TypeVariable "a" :=: FBool, TypeVariable "b" :=: FBool]
    it "[a :=: Bool, a :=: Integer] DOES NOT unify" $ do
      unify [TypeVariable "a" :=: FBool, TypeVariable "a" :=: FInteger] `shouldBe` Nothing