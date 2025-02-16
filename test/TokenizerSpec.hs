module TokenizerSpec where

import Data.Either (isRight)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Tokenizer (tokenize)

spec :: Spec
spec = do
  describe "tokenizer tests" $ do
    it "can tokenize a list of all tokens separated by spaces" $ tokenize "; = let in if then else & | == < + - * / not ( ) main 123 true" `shouldSatisfy` isRight
    it "can tokenize boolfak example program" $ tokenize "bool x = x == true | x == false; f x = if bool x | x < 0 then 1 else x * f (x - 1); main = f 6;" `shouldSatisfy` isRight
    it "can tokenize fak example program" $ tokenize "f x = if x < 0 then 1 else x * f(x - 1); main = f 6;" `shouldSatisfy` isRight
    it "can tokenize let example program" $ tokenize "f x = let y = x ; x = 5 in y; main = (f 1);" `shouldSatisfy` isRight
    it "can tokenize quadrat example program" $ tokenize "main = quadrat (quadrat (3 * 1)); quadrat x = x * x;" `shouldSatisfy` isRight
    it "can tokenize second example program" $ tokenize "main = second 1 2; second x y = y;" `shouldSatisfy` isRight