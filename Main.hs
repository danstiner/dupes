
import System.Environment

import Traverser
import qualified Store.Flat as Flat

main :: IO ()
main = do
  print "Hello Cloud World"
  [path] <- getArgs
  traverseAndPrint path
--  fileStore <- traverseAndStore path InMem.createStore
--  print fileStore
  traverseAndStoreFlat path $ Flat.createStore "/home/stiner/.bitcloud"
