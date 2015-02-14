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
  ) where

import           DuplicateCache               (DuplicateCache)
import qualified DuplicateCache
import           FileAccess                   (FileAccess)
import qualified FileAccess
import           Index                        (Index)
import qualified Index

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Database.LevelDB             (Options (..))
import qualified Database.LevelDB             as DB
import qualified Database.LevelDB.Base        as DBB
import           Pipes
import           System.Directory
import           System.FilePath
import           System.Log.Logger

logTag :: String
logTag = "Repository"

newtype Store = Store { getStorePath :: FilePath }
data Repository = Repository { getPath :: FilePath, getStore :: Store }
data RepositoryHandle = RepositoryHandle { getIndex :: Index, getCache :: DuplicateCache }

get :: IO Repository
get = findPath >>= repoAt

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
isRepository = FileAccess.doesDirectoryExist . repoDirFor

repoDirFor :: FilePath -> FilePath
repoDirFor = (</> ".dupes")

repoAt :: FilePath -> IO Repository
repoAt path = return $ Repository path (Store (repoDirFor path </> "store"))

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
  where
    repoPath = repoDirFor path
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
