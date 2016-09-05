module Main where

import qualified Database.SQLite
import qualified Dupes.FileHash
import qualified Dupes.Index.Internal.SQLite
import qualified Dupes.Repository            as Repository
import qualified Keep
import qualified PathSpec
import qualified Pipes.SQLite.Simple
import qualified Store

import           Test.Tasty                  (defaultMain, testGroup)

main :: IO ()
main = defaultMain $
  testGroup "Library tests"
    [ PathSpec.pureTestGroup
    , Repository.pureTestGroup
    , Keep.pureTests
    , Store.pureTests
    , Database.SQLite.integrationTests
    , Pipes.SQLite.Simple.integrationTests
    , Dupes.FileHash.integrationTests
    , Dupes.Index.Internal.SQLite.integrationTests
    ]
