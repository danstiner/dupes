{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Repository (
    Store (..)
  , Repository (..)
  , RepositoryHandle (..)
  , get
  , findPath
  , update
  , withRepository
  , create
  , find
  , isRepository
  , Repository.runEffect
  , htf_thisModulesTests
  ) where

import           DuplicateCache               (DuplicateCache)
import qualified DuplicateCache
import           FileAccess                   (FileAccess)
import qualified FileAccess
import           Index                        (Index)
import qualified Index

import           Data.Either.Compat
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Database.LevelDB             (Options (..))
import qualified Database.LevelDB             as DB
import qualified Database.LevelDB.Base        as DBB
import           Pipes
import           System.Directory
import           System.FilePath
import           System.Log.Logger

import Test.Framework
import Test.HUnit

logTag :: String
logTag = "Repository"

newtype Store = Store { getStorePath :: FilePath }
data Repository = Repository { getPath :: FilePath, getStore :: Store }
data RepositoryHandle = RepositoryHandle { getIndex :: Index, getCache :: DuplicateCache }

get :: IO Repository
get = findPath >>= getRepoAt

findPath :: IO FilePath
findPath = getCurrentDirectory >>= findRepo

findRepo :: FilePath -> IO FilePath
findRepo path =
    FileAccess.runIO (find path) >>= either noRepoFound return
  where
    noRepoFound = errorAndCrash
    errorAndCrash msg = errorM logTag msg >> fail msg

find :: FilePath -> FileAccess (Either String FilePath)
find path = isRepository path >>= pathIsRepo
  where
    pathIsRepo True  = return $ Right path
    pathIsRepo False = do
      parent <- FileAccess.parentDirectory path
      if path == parent
        then reachedRootPath
        else find parent
    reachedRootPath = return $ Left "Neither path or any of its parents are a repository"

isRepository :: FilePath -> FileAccess Bool
isRepository = FileAccess.doesDirectoryExist . repositorySubdir

repositorySubdir :: FilePath -> FilePath
repositorySubdir = (</> ".dupes")

getRepoAt :: FilePath -> IO Repository
getRepoAt path = return $ Repository path (Store (repositorySubdir path </> "store"))

update :: MonadResource m => RepositoryHandle -> m ()
update r = Pipes.runEffect $ Index.update (getIndex r) >-> DuplicateCache.update (getCache r)

open :: MonadResource m => Repository -> m RepositoryHandle
open (Repository path (Store store)) = do
  db <- DB.open store DB.defaultOptions
  return $ RepositoryHandle (Index.open db path) (DuplicateCache.open db)

create :: FilePath -> IO ()
create path = do
  exists <- FileAccess.runIO $ isRepository path
  unless exists $ do
    createDirectory repoPath
    createStore repoPath
    infoM logTag ("Initialized empty repository at " ++ repoPath)
  where
    repoPath = repositorySubdir path
    createStore path' = DBB.withDB (path' </> "store") (DB.defaultOptions {createIfMissing=True}) (const $ return ())

withRepository :: MonadResource m => Repository -> (RepositoryHandle -> m a) -> m a
withRepository r f = open r >>= f

runEffect :: (RepositoryHandle -> Effect (ResourceT IO) a) -> IO a
runEffect f = get >>= \r -> runEffectWith r f

runEffectWith :: Repository -> (RepositoryHandle -> Effect (ResourceT IO) a) -> IO a
runEffectWith r effect = runRepositoryAction (Pipes.runEffect . effect)
  where
    runRepositoryAction :: (RepositoryHandle -> ResourceT IO a) -> IO a
    runRepositoryAction action = runResourceT $ withRepository r action


test_isRepository = True @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = testRepoDirAt "/path/"

test_isNotRepository = False @=? result
  where
    result = FileAccess.runPure filesystem $ isRepository "/path"
    filesystem = []

test_findWhenDirectoryIsRepo = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ find "/path"
    filesystem = testRepoDirAt "/path/"

test_findWhenParentIsRepo = expected @=? actual
  where
    expected = Right "/path"
    actual = FileAccess.runPure filesystem $ find "/path/inner"
    filesystem = testRepoDirAt "/path/"

test_findWhenRootIsRepo = expected @=? actual
  where
    expected = Right "/"
    actual = FileAccess.runPure filesystem $ find "/path"
    filesystem = testRepoDirAt "/"

test_findWhenNoRepo = True @=? isLeft result
  where
    result = FileAccess.runPure filesystem $ find "/path"
    filesystem = ["/path", "/"]

testRepoDirAt :: FilePath -> [FilePath]
testRepoDirAt path = [path </> ".dupes"]
