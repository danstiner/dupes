
import System.Environment

import Traverser
import InMem

main :: IO ()
main = do
  print "Hello Cloud World"
  [path] <- getArgs
  traverseAndPrint path
--  fileStore <- traverseAndStore path createStore
--  print fileStore
