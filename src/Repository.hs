{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Repository (
    Store (..)
  , Repository (..)
  , create
  , get
  , findPath
  , find
  , isRepository
  , htf_thisModulesTests
  ) where

import           FileAccess                   (FileAccess)
import qualified FileAccess

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Either.Compat
import           Pipes
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
create = getCurrentDirectory >>= createAt

createAt :: FilePath -> IO Repository
createAt path = do
    exists <- FileAccess.runIO $ isRepository path
    unless exists $ do
      createDirectory storePath
      infoM logTag ("Initialized empty repository at " ++ path)
    getRepoAt path
  where
    storePath = repositorySubdir path

get :: IO Repository
get = findPath >>= getRepoAt

findPath :: IO FilePath
findPath = getCurrentDirectory >>= findRepo

findRepo :: FilePath -> IO FilePath
findRepo path =
    FileAccess.runIO (find path) >>= either noRepoFound return
  where
    noRepoFound = errorAndCrash
    errorAndCrash msg = errorM logTag msg >> exitFailure

find :: FilePath -> FileAccess (Either String FilePath)
find path = isRepository path >>= pathIsRepo
  where
    pathIsRepo True  = return $ Right path
    pathIsRepo False = do
      parent <- FileAccess.parentDirectory path
      if path == parent
        then reachedRootPath
        else find parent
    reachedRootPath = return $ Left "Neither path nor any of its parents are a repository"

isRepository :: FilePath -> FileAccess Bool
isRepository = FileAccess.doesDirectoryExist . repositorySubdir

repositorySubdir :: FilePath -> FilePath
repositorySubdir = (</> ".dupes")

getRepoAt :: FilePath -> IO Repository
getRepoAt path = return $ Repository path (Store (repositorySubdir path </> "store"))

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
    actual = FileAccess.runPure filesystem $ find "/path"
    filesystem = testRepoDirAt "/path"

test_findWhenParentIsRepo = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ find "/path/inner"
    filesystem = testRepoDirAt "/path"

test_findWhenRootIsRepo = expected @=? actual
  where
    expected = Right "/"
    actual = FileAccess.runPure filesystem $ find "/path"
    filesystem = testRepoDirAt "/"

test_findWhenNoRepo :: Assertion
test_findWhenNoRepo = True @=? isLeft result
  where
    result = FileAccess.runPure filesystem $ find "/path"
    filesystem = ["/path", "/"]

testRepoDirAt :: FilePath -> [FilePath]
testRepoDirAt path = [path </> ".dupes"]
