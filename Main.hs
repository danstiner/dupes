
import System.Environment

import qualified Plumbing
import qualified Store.Mem as Mem
import qualified Store.Flat as Flat
import qualified Store.LevelDB as LevelDB
import qualified Store.RemoteHttp as Remote
import System.Directory as Dir

main :: IO ()
main = do
  [path] <- getArgs
  home <- Dir.getHomeDirectory
  treeId <- Plumbing.writeTree path $ Flat.createStore (home ++ "/.bitcloud")
  Plumbing.setRef "master" treeId $ LevelDB.createStore (home ++ "/.bitcloud/leveldb")
  return ()
  