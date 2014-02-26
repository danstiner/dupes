
import System.Environment

import Traverser
import qualified Store.Flat as Flat
import System.Directory as Dir

main :: IO ()
main = do
  print "Hello Cloud World"
  [path] <- getArgs
--  fileStore <- traverseAndStore path InMem.createStore
--  print fileStore
  home <- Dir.getHomeDirectory
  traverseAndStoreFlat path $ Flat.createStore (home ++ "/.bitcloud")
