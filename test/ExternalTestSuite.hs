module Main where

import qualified Tests.Store.LevelDB as LevelDB
import           Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Store.LevelDB" LevelDB.externalTests ]
