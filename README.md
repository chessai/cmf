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
-- Example 1: Grabbing data from a bunch of nodes (represented as just IPv4 addresses)
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

```haskell
-- Example 2: Sending out emails concurrently
module Main (main) where

-- intended to be imported qualified
import qualified Cmf
import Data.Map (Map)

main :: IO ()
main = do
  emails <- getWhoShouldBeNotifiedOnEvent AccountSetupFinished
  errs <- Cmf.foldMapWithKey (\k v -> (:[]) <$> sendEmail k v) emails
  print errs

-- opaque data types, address and content of email
data EmailAddress = EmailAddress
  deriving (Show)
data EmailContent = EmailContent
  deriving (Show)
-- failed to send the email - address and error message
data EmailError = EmailError EmailAddress String
  deriving (Show)

sendEmail :: EmailAddress -> EmailContent -> IO (Either EmailError ())
sendEmail = ...

data Event = AccountSetupFinished | SomethingElse

getWhoShouldBeNotifiedOnEvent :: Event -> IO (Map EmailAddress EmailContent)
getWhoShouldBeNotifiedOnEvent = ...
```
