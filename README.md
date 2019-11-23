cmf
===

## What is it
This library provides concurrent folds for commutative monoids.

This is useful when you have some IO-bound task which meets at
least the following two conditions:

1. It needs to be run over potentially many of the same type of input
2. The output can be combined in a commutative fashion

## How to use it

```haskell
{-# language PackageImports #-}

module Main (main) where

-- intended to be imported qualified
import qualified Cmf
import "ip" Net.IPv4 (IPv4)

main :: IO ()
main = do
  nodes <- readNodesFromFile "some_nodes.txt"
  -- the list monoid is 'commutative' if we don't care about ordering
  someData <- Cmf.foldMap reachOutToNodeAndGetData nodes
  print someData

data SomeData = SomeData
  deriving (Show)

reachOutToNodeAndGetData :: IPv4 -> IO [SomeData]
reachOutToNodeAndGetData = ...

readNodesFromFile :: FilePath -> IO [IPv4]
readNodesFromFile = ...
```
