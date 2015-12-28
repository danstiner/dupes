module Main where

import qualified Repository

import Test.Framework                    (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit                        hiding (Test)

main :: IO ()
main = defaultMain [ Repository.pureTestGroup ]
