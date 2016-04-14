{-# LANGUAGE TemplateHaskell #-}

module Dupes.Repository (
    Repository(..),
    create,
    find,
    isRepository,
    pureTestGroup,
    ) where

import           FileAccess                   (FileAccess)
import qualified FileAccess

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Either.Compat
import qualified Data.List                    as List
import           Data.Maybe                   (listToMaybe)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Log.Logger

import           Test.Tasty.HUnit
import           Test.Tasty.TH

logTag :: String
logTag = "Repository"

data Repository = Repository { getWorkingDirectory :: FilePath, getPath :: FilePath }

create :: IO Repository
create = getCurrentDirectory >>= FileAccess.runIO . createAt

createAt :: FilePath -> FileAccess Repository
createAt path = do
  let storePath = repositorySubdir path
  FileAccess.createDirectoryIfMissing storePath
  return $ getRepoAt path

find :: IO Repository
find = getRepoAt <$> findFromCurrentDirectory

findFromCurrentDirectory :: IO FilePath
findFromCurrentDirectory =
  getCurrentDirectory >>= findFromIO >>= either noRepoFound return
  where
    noRepoFound = errorAndCrash
    errorAndCrash msg = errorM logTag ("Failed to find repository from current directory: " ++ msg) >> exitFailure

findFromIO :: FilePath -> IO (Either String FilePath)
findFromIO = FileAccess.runIO . findFrom

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
getRepoAt path = Repository path (repositorySubdir path)

case_isRepository_for_repo_path_is_True = True @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = fakeRepoDirAt "/path"

case_isRepository_for_non_repo_path_is_False = False @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = []

case_findFrom_when_directory_is_repository = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ findFrom "/path"
    filesystem = fakeRepoDirAt "/path"

case_findFrom_when_parent_directory_is_repository = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ findFrom "/path/inner"
    filesystem = fakeRepoDirAt "/path"

case_findFrom_when_root_is_repository = expected @=? actual
  where
    expected = Right "/"
    actual = FileAccess.runPure filesystem $ findFrom "/path"
    filesystem = fakeRepoDirAt "/"

case_findFrom_when_no_repo_isLeft = True @=? isLeft result
  where
    result = FileAccess.runPure filesystem $ findFrom "/path"
    filesystem = ["/path", "/"]

case_createAt_creates_a_repository = True @=? result
  where
    filesystem = ["/path", "/"]
    result = FileAccess.runPure filesystem $ do
      createAt "/path"
      isRepository "/path"

case_createAt_is_idempotent = True @=? result
  where
    filesystem = ["/path", "/"] ++ fakeRepoDirAt "/path"
    result = FileAccess.runPure filesystem $ do
      createAt "/path"
      isRepository "/path"

fakeRepoDirAt :: FilePath -> [FilePath]
fakeRepoDirAt path = [path </> ".dupes"]

pureTestGroup = $(testGroupGenerator)
