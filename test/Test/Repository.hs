module Test.Repository (tests) where

import           FileAccess
import           Repository

import           Data.Either
import           System.FilePath
import           Test.Framework.Providers.HUnit
import           Test.HUnit

tests =
    [ testCase "isRepository" test_isRepository
    , testCase "isNotRepository" test_isNotRepository
    , testCase "findWhenDirectoryIsRepo" test_findWhenDirectoryIsRepo
    , testCase "findWhenParentIsRepo" test_findWhenParentIsRepo
    , testCase "findWhenRootIsRepo" test_findWhenRootIsRepo
    , testCase "findWhenNoRepo" test_findWhenNoRepo
    ]

test_isRepository = True @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = makeRepository "/path/"

test_isNotRepository = False @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = []

test_findWhenDirectoryIsRepo = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ find "/path"
    filesystem = makeRepository "/path/"

test_findWhenParentIsRepo = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ find "/path/inner"
    filesystem = makeRepository "/path/"

test_findWhenRootIsRepo = expected @=? actual
  where
    expected = Right "/"
    actual = FileAccess.runPure filesystem $ find "/path"
    filesystem = makeRepository "/"

test_findWhenNoRepo = True @=? isLeft result
  where
    result = FileAccess.runPure filesystem $ find "/path"
    filesystem = ["/path", "/"]

isLeft = either (const True) (const False)

makeRepository path = [path </> ".dupes"]
