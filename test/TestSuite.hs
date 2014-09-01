module Main where

import qualified Tests.Command.Dupes as Dupes
import qualified Tests.Store.Mem as Mem
import           Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Command.Dupes" Dupes.tests
            , testGroup "Tests.Store.Mem" Mem.tests
            ]
