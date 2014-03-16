
import System.Environment

import qualified Plumbing
import qualified Store.Mem as Mem
import qualified Store.Flat as Flat
import qualified Store.RemoteHttp as Remote
import System.Directory as Dir

main :: IO ()
main = do
  [path] <- getArgs
  home <- Dir.getHomeDirectory
  Plumbing.writeTree path $ Mem.createStore
  Plumbing.writeTree path $ Flat.createStore (home ++ "/.bitcloud")