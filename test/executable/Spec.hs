module Main where

import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $
  testGroup "Executable tests" []
