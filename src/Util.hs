module Util (
  getStore
) where

import qualified Settings
import           Store.LevelDB   as LevelDB

import           System.FilePath ((</>))

getStore :: IO LevelDB.Store
getStore = do
  appDir <- Settings.getAppDir
  return $ LevelDB.createStore (appDir </> "leveldb")
