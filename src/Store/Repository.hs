{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Store.Repository (
    Store (..)
  , Repository (..)
  , RepositoryHandle (..)
  , get
  , findPath
  , update
  , withRepository
  , create
  ) where

import qualified Index as Index
import qualified DuplicateCache as DuplicateCache
import Index (Index)
import DuplicateCache (DuplicateCache)

import Control.Applicative
import qualified Database.LevelDB as DB
import qualified Database.LevelDB.Base as DBB
import Database.LevelDB (runResourceT, MonadResource, Options (..))
import           Control.Monad
import           System.Directory
import           System.FilePath
import           System.Log.Logger
import           Pipes
import           Control.Exception

logTag :: String
logTag = "Store.Repository"

newtype Store = Store { getStorePath :: FilePath }
data Repository = Repository { getPath :: FilePath, getStore :: Store }
data RepositoryHandle = RepositoryHandle { getIndex :: Index, getCache :: DuplicateCache }

get :: IO Repository
get = findPath >>= repoAt

findPath :: IO FilePath
findPath = getCurrentDirectory >>= findRepo

-- TODO: Should work similar to how git files either .git files containing the filename of the config directory
--       or actual .git directories
findRepo :: FilePath -> IO FilePath
findRepo dir = do
    exists <- doesDirectoryExist repoPath
    if exists
      then return dir
      else do
        when (isRoot dir) $ errorAndCrash "fatal: Not a dupes repository (or any of the parent directories)"
        findRepo (takeDirectory dir)
  where
    repoPath = repoDirFor dir
    isRoot path = takeDirectory path == path
    errorAndCrash msg = errorM logTag msg >> fail msg

repoDirFor = (</> ".dupes")

repoAt :: FilePath -> IO Repository
repoAt path = return $ Repository path (Store (repoDirFor path </> "store"))

update :: (MonadResource m) => RepositoryHandle -> m ()
update r = runEffect $ Index.update (getIndex r) >-> DuplicateCache.update (getCache r)

open :: (MonadResource m) => Repository -> m RepositoryHandle
open (Repository path (Store store)) = do
  db <- DB.open store DB.defaultOptions
  return $ RepositoryHandle (Index.open db path) (DuplicateCache.open db)

create :: FilePath -> IO ()
create path = do
    createDirectory repoPath
    DBB.withDB (repoPath </> "store") (DB.defaultOptions {createIfMissing=True}) (\db -> return ())
  where
    repoPath = repoDirFor path 

withRepository :: MonadResource m => Repository -> (RepositoryHandle -> m a) -> m a
withRepository r f = open r >>= f
