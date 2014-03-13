
module Plumbing (
writeTree,
) where

import qualified Data.ByteString as B
import Control.Monad ( filterM )
import Control.Monad.Trans.State (evalStateT)
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath ( (</>) )

import Store.Blob as BlobStore
import qualified Blob
import qualified Tree

writeTree :: (BlobStore a) => FilePath -> a -> IO ()
writeTree path store = do
  names <- getDirectoryContents path
  let properNames = filter (`notElem` [".", ".."]) names
  filenames <- filterM notDir properNames
  entries <- mapM toEntry filenames
  let tree = Tree.create entries
  saveTree tree store
  return ()

  where
  	notDir name = do
  		isDir <- doesDirectoryExist (path </> name)
  		return $ not isDir
  	toEntry name = createTreeEntry path name store

pathToBlob :: FilePath -> IO Blob.Blob
pathToBlob path = do
  contents <- B.readFile path
  return $ Blob.create contents

createTreeEntry :: (BlobStore a) => FilePath -> String -> a -> IO Tree.TreeEntry
createTreeEntry parentPath name store = do
  let filepath = parentPath </> name
  blob <- pathToBlob filepath
  evalStateT (BlobStore.put blob) store
  let (Blob.Blob key _) = blob
  return $ Tree.createEntry name 0 key

saveTree :: (BlobStore a) => Tree.Tree -> a -> IO ()
saveTree tree store = do
  let blob = Tree.toBlob tree
  evalStateT (BlobStore.put blob) store
  