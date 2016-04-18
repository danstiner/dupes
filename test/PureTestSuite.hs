module Main where

import qualified Dupes.Repository as Repository
import qualified Keep
import qualified PathSpec
import qualified Store

import           Test.Tasty       (defaultMain, testGroup)

main :: IO ()
main = defaultMain $
  testGroup "Pure tests"
    [PathSpec.pureTestGroup, Repository.pureTestGroup, Keep.pureTests, Store.pureTests]
