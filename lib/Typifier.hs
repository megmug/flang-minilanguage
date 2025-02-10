{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Typifier where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (State, evalState)
import SyntaxTree (Program, Stage (Core))

newtype TypifierState = TypifierState ()

data FLangType = FBool | FInteger deriving (Eq, Show)

type Typifier = ExceptT String (State TypifierState) FLangType

{- Type class for syntactical elements that can be typified -}
class Typifiable a where
  -- A typifier creates a monadic action from a syntactical element that can generate code for it
  typifier :: a -> Typifier

  -- This runs a typifier with some supplied state (can also be useful for testing)
  customTypify :: a -> TypifierState -> Either String FLangType
  customTypify e = evalState (runExceptT $ typifier e)

  -- This runs a typifier with some default empty state (mostly useful for whole programs)
  typify :: a -> Either String FLangType
  typify e = customTypify e $ TypifierState ()

instance Typifiable (Program Core) where
  typifier _ = return FInteger