
import System.Environment

import qualified App
import qualified Logging
import qualified Plumbing
import qualified Store.Mem as Mem
import qualified Store.Flat as Flat
import qualified Store.LevelDB as LevelDB
import qualified Store.RemoteHttp as Remote
import qualified Telemetry
import System.Directory as Dir
import System.Log.Logger
import System.FilePath ( (</>) )
import System.Directory as Directory

main :: IO ()
main = do
  home <- Dir.getHomeDirectory
  let appUserDir = home </> ".clod"
  Directory.createDirectoryIfMissing False appUserDir


  Logging.registerLogger appUserDir WARNING
  Telemetry.registerLogger appUserDir

  infoM App.logTag "Application launching"

  [path] <- getArgs
  treeId <- Plumbing.writeTree path $ Flat.createStore (appUserDir)
  Plumbing.setRef "master" treeId $ LevelDB.createStore (appUserDir </> "leveldb")
  return ()
