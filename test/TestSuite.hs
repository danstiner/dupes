module Main where

import qualified Repository

import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $
  testGroup "Pure tests" [Repository.pureTestGroup]
