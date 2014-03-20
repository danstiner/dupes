
import System.Environment

import qualified Plumbing
import qualified Store.Mem as Mem
import qualified Store.Flat as Flat
import qualified Store.LevelDB as LevelDB
import qualified Store.RemoteHttp as Remote
import System.Directory as Dir
import System.FilePath ( (</>) )
import System.Directory as Directory

main :: IO ()
main = do
  home <- Dir.getHomeDirectory
  let appUserDir = home </> ".clod"
  Directory.createDirectoryIfMissing False appUserDir
  [path] <- getArgs
  treeId <- Plumbing.writeTree path $ Flat.createStore (appUserDir)
  Plumbing.setRef "master" treeId $ LevelDB.createStore (appUserDir </> "leveldb")
  return ()
  