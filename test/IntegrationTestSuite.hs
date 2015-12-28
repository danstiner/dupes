module Main where

import qualified Database.SQLite as SQLite

import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Integration tests" [SQLite.integrationTests]
