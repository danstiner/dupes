module Main where

import qualified Database.SQLite
import qualified Dupes.FileHash
import qualified Dupes.Index.Internal.SQLite
import qualified Pipes.SQLite.Simple

import           Test.Tasty          (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Integration tests"
                       [ Database.SQLite.integrationTests
                       , Pipes.SQLite.Simple.integrationTests
                       , Dupes.FileHash.integrationTests
                       , Dupes.Index.Internal.SQLite.integrationTests
                       ]
