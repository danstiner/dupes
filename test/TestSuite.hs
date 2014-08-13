module Main where

import qualified Tests.Store.Mem as MemStore
import qualified Tests.Command.Dupes as Dupes
import           Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Store.Mem" MemStore.tests
            , testGroup "Tests.Command.Dupes" Dupes.tests]
