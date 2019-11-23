{-# language TemplateHaskell #-}

module Main (main) where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid (Sum(..))

import qualified Cmf
import qualified Data.Map as Map

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Prelude
import qualified Prelude

main :: IO Bool
main = checkParallel $$(discover)

prop_foldMapWithKey :: Property
prop_foldMapWithKey = property $ do
  let genKey = Gen.int Range.constantBounded
  let genVal = Gen.list (Range.linear 5 25) genKey
  m <- forAll $ Gen.map (Range.linear 50 200) ((,) <$> genKey <*> genVal)
  let f k v = pure $ Sum $ k * sum v
  lhs <- liftIO $ Map.foldMapWithKey f m
  rhs <- liftIO $ Cmf.foldMapWithKey f m
  lhs === rhs

prop_foldMap :: Property
prop_foldMap = property $ do
  let genKey = Gen.int Range.constantBounded
  let genVal = Gen.list (Range.linear 5 25) genKey
  l <- forAll $ Gen.list (Range.linear 50 200) ((,) <$> genKey <*> genVal)
  let f k v = pure $ Sum $ k * sum v
  lhs <- liftIO $ Prelude.foldMap (uncurry f) l
  rhs <- liftIO $ Cmf.foldMap (uncurry f) l
  lhs === rhs
