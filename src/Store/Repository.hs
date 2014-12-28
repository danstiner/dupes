module Store.Repository (
    Store (..)
  , Repository (..)
  , get
  , getPath
) where

import qualified App

import Control.Monad
import System.Directory
import System.FilePath
import System.Log.Logger

logTag :: String
logTag = App.logTag ++ ".Store.Repository"

newtype Store = Store { getStorePath :: FilePath }
data Repository = Repository { getStore :: Store }

get :: IO Repository
get = getPath >>= repoAt

getPath :: IO FilePath
getPath = getCurrentDirectory >>= findRepo

findRepo :: FilePath -> IO FilePath
findRepo dir = do
    exists <- doesDirectoryExist repoPath
    if exists
      then return repoPath
      else do
        when (isRoot dir) $ errorAndCrash "fatal: Not a dupes repository (or any of the parent directories)"
        findRepo (takeDirectory dir)
  where
    repoPath = dir </> ".dupes"
    isRoot path = takeDirectory path == path
    errorAndCrash msg = errorM logTag msg >> fail msg

repoAt :: FilePath -> IO Repository
repoAt path = return $ Repository (Store (path </> "store"))
