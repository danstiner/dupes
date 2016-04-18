module Main where

import qualified Database.SQLite     as SQLite
import qualified Pipes.SQLite.Simple

import           Test.Tasty          (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Integration tests"
                       [SQLite.integrationTests, Pipes.SQLite.Simple.integrationTests]
