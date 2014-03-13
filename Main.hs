
import System.Environment

import qualified Plumbing
import qualified Store.Mem as Mem
import qualified Store.Flat as Flat
import System.Directory as Dir

main :: IO ()
main = do
  print "Hello Cloud World"
  [path] <- getArgs
--  fileStore <- traverseAndStore path InMem.createStore
--  print fileStore
  home <- Dir.getHomeDirectory
--  traverseAndStoreFlat path $ Flat.createStore (home ++ "/.bitcloud")
  Plumbing.writeTree path $ Mem.createStore
  Plumbing.writeTree path $ Flat.createStore (home ++ "/.bitcloud")