{-# LANGUAGE TemplateHaskell #-}

module Dupes.Repository (
    Repository(..),
    initialize,
    findFrom,
    isRepository,
    pureTestGroup,
    ) where

import           Control.Monad
import           Data.Maybe
import           Dupes.Index                  as Index
import           Dupes.WorkingDirectory       as WorkingDirectory
import           FileAccess                   (FileAccess)
import qualified FileAccess
import           System.FilePath

import           Test.Tasty.HUnit
import           Test.Tasty.TH

data Repository = Repository { workingDirectory :: WorkingDirectory, indexPath :: IndexPath }
  deriving Show

initialize :: FilePath -> IO Repository
initialize = FileAccess.runIO . initializeF

findFrom :: FilePath -> IO (Maybe Repository)
findFrom path = (fmap . fmap) getAt (findPathFrom path)

isRepository :: FilePath -> IO Bool
isRepository = FileAccess.runIO . isRepositoryF

initializeF :: FilePath -> FileAccess Repository
initializeF path = do
  let repositoryPath = repositorySubdirectory path
  FileAccess.createDirectoryIfMissing repositoryPath
  return $ getAt path

findPathFrom :: FilePath -> IO (Maybe FilePath)
findPathFrom = FileAccess.runIO . findPathFromF

findPathFromF :: FilePath -> FileAccess (Maybe FilePath)
findPathFromF = findM isRepositoryF <=< FileAccess.parentDirectories

findM :: (Monad m, Functor m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f = fmap listToMaybe . filterM f

isRepositoryF :: FilePath -> FileAccess Bool
isRepositoryF = FileAccess.doesDirectoryExist . repositorySubdirectory

repositorySubdirectory :: FilePath -> FilePath
repositorySubdirectory = (</> ".dupes")

getAt :: FilePath -> Repository
getAt path = Repository (WorkingDirectory.construct path)
               (Index.construct (repositorySubdirectory path))

case_isRepository_for_repository_is_True = True @=? result
  where
    result = FileAccess.runPure filesystem $ isRepositoryF "/path"
    filesystem = fakeRepositoryAt "/path"

case_isRepository_for_non_repo_path_is_False = False @=? result
  where
    result = FileAccess.runPure filesystem $ isRepositoryF "/path"
    filesystem = []

case_findFrom_when_directory_is_repository = expected @=? actual
  where
    expected = Just "/path"
    actual = FileAccess.runPure filesystem $ findPathFromF "/path"
    filesystem = fakeRepositoryAt "/path"

case_findFrom_when_parent_directory_is_repository = expected @=? actual
  where
    expected = Just "/path"
    actual = FileAccess.runPure filesystem $ findPathFromF "/path/inner"
    filesystem = fakeRepositoryAt "/path"

case_findFrom_when_root_is_repository = expected @=? actual
  where
    expected = Just "/"
    actual = FileAccess.runPure filesystem $ findPathFromF "/path"
    filesystem = fakeRepositoryAt "/"

case_findFrom_when_no_repository_is_Nothing = True @=? isNothing result
  where
    result = FileAccess.runPure filesystem $ findPathFromF "/path"
    filesystem = ["/path", "/"]

case_initialize_creates_a_repository = True @=? result
  where
    filesystem = ["/path", "/"]
    result = FileAccess.runPure filesystem $ do
      _ <- initializeF "/path"
      isRepositoryF "/path"

case_initialize_is_idempotent = True @=? result
  where
    filesystem = ["/path", "/"] ++ fakeRepositoryAt "/path"
    result = FileAccess.runPure filesystem $ do
      _ <- initializeF "/path"
      isRepositoryF "/path"

fakeRepositoryAt :: FilePath -> [FilePath]
fakeRepositoryAt path = [repositorySubdirectory path]

pureTestGroup = $(testGroupGenerator)
