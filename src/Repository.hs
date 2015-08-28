{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Repository (
    Store (..)
  , Repository (..)
  , create
  , find
  , findFrom
  , isRepository
  , htf_thisModulesTests
  ) where

import           FileAccess                   (FileAccess)
import qualified FileAccess

import Data.Maybe (listToMaybe)
import qualified Data.List as List
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Either.Compat
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Log.Logger

import           Test.Framework
import           Test.HUnit

logTag :: String
logTag = "Repository"

newtype Store = Store { getStorePath :: FilePath }

data Repository = Repository { getPath :: FilePath, getStore :: Store }

create :: IO Repository
create = getCurrentDirectory >>= FileAccess.runIO . createAt

createAt :: FilePath -> FileAccess Repository
createAt path = do
  let storePath = repositorySubdir path
  FileAccess.createDirectoryIfMissing storePath
  return $ getRepoAt path

find :: IO Repository
find = findPath >>= return . getRepoAt

findPath :: IO FilePath
findPath = getCurrentDirectory >>= findRepo

findRepo :: FilePath -> IO FilePath
findRepo path =
    FileAccess.runIO (findFrom path) >>= either noRepoFound return
  where
    noRepoFound = errorAndCrash
    errorAndCrash msg = errorM logTag msg >> exitFailure

findFrom :: FilePath -> FileAccess (Either String FilePath)
findFrom = return . eitherFailedOrFound <=< findM isRepository <=< FileAccess.parentDirectories
  where
    eitherFailedOrFound = maybe failedToFind Right
    failedToFind = Left "Neither path nor any of its parents are a repository"

findM :: (Monad m, Functor m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f = fmap listToMaybe . filterM f

isRepository :: FilePath -> FileAccess Bool
isRepository = FileAccess.doesDirectoryExist . repositorySubdir

repositorySubdir :: FilePath -> FilePath
repositorySubdir = (</> ".dupes")

getRepoAt :: FilePath -> Repository
getRepoAt path = Repository path (Store (repositorySubdir path </> "store"))

test_isRepository = True @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = testRepoDirAt "/path"

test_isNotRepository = False @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = []

test_findWhenDirectoryIsRepo = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ findFrom "/path"
    filesystem = testRepoDirAt "/path"

test_findWhenParentIsRepo = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ findFrom "/path/inner"
    filesystem = testRepoDirAt "/path"

test_findWhenRootIsRepo = expected @=? actual
  where
    expected = Right "/"
    actual = FileAccess.runPure filesystem $ findFrom "/path"
    filesystem = testRepoDirAt "/"

test_findWhenNoRepo :: Assertion
test_findWhenNoRepo = True @=? isLeft result
  where
    result = FileAccess.runPure filesystem $ findFrom "/path"
    filesystem = ["/path", "/"]

test_createAt_createsRepository :: Assertion
test_createAt_createsRepository = True @=? result
  where
    filesystem = ["/path", "/"]
    result = FileAccess.runPure filesystem $ do
      createAt "/path"
      isRepository "/path"

test_createAt_isIdempotent :: Assertion
test_createAt_isIdempotent = True @=? result
  where
    filesystem = ["/path", "/"] ++ testRepoDirAt "/path"
    result = FileAccess.runPure filesystem $ do
      createAt "/path"
      isRepository "/path"

testRepoDirAt :: FilePath -> [FilePath]
testRepoDirAt path = [path </> ".dupes"]
