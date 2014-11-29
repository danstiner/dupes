module Main where

import           Test.Framework      (defaultMain, testGroup)
import qualified Tests.Command.Dupes as Dupes
import qualified Tests.Machine       as Machine
import qualified Tests.Store.LevelDB as LevelDB
import qualified Tests.Store.Mem     as Mem

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "Tests.Command.Dupes" Dupes.tests
            , testGroup "Tests.Machine" Machine.tests
            , testGroup "Tests.Store.Mem" Mem.tests
            , testGroup "Tests.Store.LevelDB" LevelDB.tests
            ]
