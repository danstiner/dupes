
module Traverser (
  traverseAndStore,
  traverseAndStoreFlat,
  traverseAndPrint
) where

import Store.Mem as InMem
import qualified Blob
import qualified Data.ByteString as B
import Store.Flat as Flat
import Store.Blob as BlobStore

import Control.Monad ( forM_ )
import Control.Proxy
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath ( (</>) )
import Control.Monad.Trans.State (evalStateT)

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

pathToBlob :: FilePath -> IO Blob.Blob
pathToBlob path = do
  contents <- B.readFile path
  return $ Blob.create contents

traverseAndStore :: FilePath -> MemStore -> IO ()
traverseAndStore path _ = 
  runProxy $ getRecursiveContents path

putHelper :: FilePath -> Flat.Store -> IO ()
putHelper path store = do
  blob <- pathToBlob path
  evalStateT (BlobStore.put blob) store

traverseAndStoreFlat :: FilePath -> Flat.Store -> IO ()
traverseAndStoreFlat path store =
  runProxy $ getRecursiveContents path
    >-> useD (\filepath -> putHelper filepath store)
