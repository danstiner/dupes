module Main where

import qualified PathSpec
import qualified Repository

import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $
  testGroup "Pure tests" [PathSpec.pureTestGroup, Repository.pureTestGroup]
