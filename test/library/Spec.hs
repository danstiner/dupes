module Main where

import qualified Database.SQLite
import qualified Dupes.FileHash
import qualified Dupes.Index.Internal.SQLite
import qualified Dupes.Repository
import qualified Keep
import qualified PathSpec
import qualified Pipes.SQLite.Simple
import qualified Store

import           Test.Tasty                  (defaultMain, testGroup)

main :: IO ()
main = defaultMain $
  testGroup "Library tests"
    [ PathSpec.testGroup
    , Dupes.Repository.testGroup
    , Keep.testGroup
    , Store.testGroup
    , Database.SQLite.testGroup
    , Pipes.SQLite.Simple.testGroup
    , Dupes.FileHash.testGroup
    , Dupes.Index.Internal.SQLite.testGroup
    ]
