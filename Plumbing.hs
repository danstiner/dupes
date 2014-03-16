
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
  tree <- createTree path store
  saveTree tree store
  return ()

createTree :: (BlobStore a) => FilePath -> a -> IO Tree.Tree
createTree path store = do
  names <- listDirectory path
  entries <- mapM toEntry names
  return $ Tree.create entries
  where
  	toEntry name = createTreeEntry path name store

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = do
	names <- getDirectoryContents path
	return $ filter (`notElem` [".", ".."]) names

createTreeEntry :: (BlobStore a) => FilePath -> FilePath -> a -> IO Tree.TreeEntry
createTreeEntry parentPath name store = do
	let fullpath = parentPath </> name
	isDir <- doesDirectoryExist fullpath
	case isDir of
		True -> createDirEntry
		False -> createFileEntry
	where
		fullpath = parentPath </> name
		createDirEntry = do
			subTree <- createTree fullpath store
			subTreeBlob <- saveTree subTree store
			let (Blob.Blob blobId _) = subTreeBlob
			return $ Tree.createEntry name 0 blobId
		createFileEntry = do
			blob <- blobFromPath fullpath
			evalStateT (BlobStore.put blob) store
			let (Blob.Blob blobId _) = blob
			return $ Tree.createEntry name 0 blobId


blobFromPath :: FilePath -> IO Blob.Blob
blobFromPath path = do
  contents <- B.readFile path
  return $ Blob.create contents

saveTree :: (BlobStore a) => Tree.Tree -> a -> IO Blob.Blob
saveTree tree store = do
  let tblob = Tree.toBlob tree
  evalStateT (BlobStore.put tblob) store
  return tblob