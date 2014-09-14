module Store.Repository (
    Store (..)
  , Repository (..)
  , get
) where

import qualified App

import System.Directory
import System.FilePath
import System.Log.Logger

logTag :: String
logTag = App.logTag ++ ".Store.Repository"

newtype Store = Store { getStorePath :: FilePath }
data Repository = Repository { getStore :: Store }

get :: IO Repository
get = getCurrentDirectory >>= get'

get' :: FilePath -> IO Repository
get' dir = do
  let rDir = dir </> ".clod"
  exists <- doesDirectoryExist rDir
  if exists
    then repoAt rDir
    else if (parent == dir)
        then errorAndCrash "fatal: Not a clod repository (or any of the parent directories)"
        else get' parent
  where
    parent = takeDirectory dir
    errorAndCrash msg = errorM logTag msg >> fail msg

repoAt :: FilePath -> IO Repository
repoAt path = return $ Repository (Store (path </> "store"))
