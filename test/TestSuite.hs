module Main where

import           Test.Repository

import           Test.Framework  (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
  where
    tests = [testGroup "Repository" Test.Repository.tests]
