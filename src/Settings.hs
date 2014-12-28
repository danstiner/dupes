module Settings (
    getAppDir
  , getAndCreateAppDir
) where

import           System.Directory as Dir
import           System.FilePath  ((</>))

getAndCreateAppDir :: IO FilePath
getAndCreateAppDir = do
  dir <- getAppDir
  Dir.createDirectoryIfMissing False dir
  return dir

getAppDir :: IO FilePath
getAppDir = fmap (</> ".dupes") Dir.getHomeDirectory
