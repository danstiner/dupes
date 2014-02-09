
module Traverser (
--traverseAndStore,
traverseAndPrint
) where

import InMem (MemStore)

import Control.Monad ( forM_, liftM )
import Control.Proxy
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.FilePath ( (</>) )

getRecursiveContents :: (Proxy p) => FilePath -> () -> Producer p FilePath IO ()
getRecursiveContents topPath () = runIdentityP $ do
  names <- lift $ getDirectoryContents topPath
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path ()
      else respond path

traverseAndPrint :: FilePath -> IO ()
traverseAndPrint path =
  runProxy $ getRecursiveContents path
        >-> useD (\file -> putStrLn $ "Found file " ++ file)

--traverseAndStore :: FilePath -> MemStore -> IO MemStore
--traverseAndStore path store = do store
