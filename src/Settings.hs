module Settings (
    getAppDir
) where

import System.Directory as Dir
import System.FilePath ( (</>) )

getAppDir :: IO FilePath
getAppDir = do
    home <- Dir.getHomeDirectory
    return $ home </> ".clod"