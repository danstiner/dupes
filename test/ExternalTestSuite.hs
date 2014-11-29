module Main where

import           Test.Framework              (defaultMain, testGroup)
import qualified Tests.Store.LevelDBExternal as LevelDB

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Store.LevelDB" LevelDB.externalTests ]
