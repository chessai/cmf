{-# language
    BangPatterns
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , LambdaCase
  , MagicHash
  , ScopedTypeVariables
  , UnboxedTuples
  #-}

-- | This module provides many concurrent monoidal folds
--   for commutative monoids.
--
--   Some notes (applies to all folds):
--
--     1. This module is intended to be imported qualified
--        to avoid name clashing.
--
--     2. Accumulation is strict.
--
--     3. Exceptions that occur will accumulate into a 'CmfException'
--        and be re-thrown.
module Cmf
  ( -- * Folds
    foldMap
  , foldMapWithKey

    -- * Exception Type
  , CmfException(..)
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import Data.Foldable (foldlM)
import Data.Monoid (Sum(..))
import GHC.Conc (ThreadId(..))
import GHC.Exts (fork#)
import GHC.Generics (Generic)
import GHC.IO (IO(..))

import qualified Data.Map as Map

import Prelude hiding (foldMap)

-- | An exception to be re-thrown by a fold in this
--   module. It is just an accumulation of all the
--   exceptions that occurred among the running
--   threads.
newtype CmfException = CmfException [SomeException]
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

-- | A concurrent monoidal fold over some 'Foldable'.
foldMap :: forall t m a. (Foldable t, Monoid m)
  => (a -> IO m)
  -> t a
  -> IO m
foldMap f xs = do
  var <- newEmptyMVar
  total <- foldlM
    (\ !n a -> do
      void $ fork $ try (f a) >>= putMVar var
      pure (n + 1)
    ) 0 xs
  internal total var
{-# inlineable foldMap #-}

-- | A concurrent monoidal fold (with keys) over a 'Map.Map'.
foldMapWithKey :: (Monoid m)
  => (k -> a -> IO m)
  -> Map.Map k a
  -> IO m
foldMapWithKey f mp = do
  var <- newEmptyMVar
  Sum total <- Map.foldMapWithKey
    (\k a -> do
      void $ fork $ try (f k a) >>= putMVar var
      pure (Sum 1)
    ) mp
  internal total var
{-# inlineable foldMapWithKey #-}

-- fork, but don't catch exceptions
-- avoids nested catch#
fork :: IO () -> IO ()
fork action = IO $ \s -> case fork# action s of
  (# s1, _ #) -> (# s1, () #)

-- worker
internal :: forall m. (Monoid m)
  => Int -- total number of threads to spawn
  -> MVar (Either SomeException m)
  -> IO m
internal total var = do
  let go2 :: Int -> [SomeException] -> IO (Either [SomeException] m)
      go2 !n !es = if n < total
        then takeMVar var >>= \case
          Left e  -> go2 (n + 1) (e:es)
          Right _ -> go2 (n + 1) es
        else pure (Left es)
  let go :: Int -> m -> IO (Either [SomeException] m)
      go !n !m = if n < total
        then takeMVar var >>= \case
          Left r -> go2 (n + 1) [r]
          Right m' -> go (n + 1) (m <> m')
        else pure (Right m)
  r <- go 0 mempty
  case r of
    Left errs -> throwIO $ CmfException errs
    Right m -> pure m
