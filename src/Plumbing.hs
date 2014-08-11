
module Plumbing (
    hashObject
  , writeTree
  , setRef
) where


import qualified App

import qualified Data.ByteString as B
import Control.Monad.Trans.State (evalStateT)
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath ( (</>) )
import System.Log.Logger
import Control.Exception
import Data.Maybe (catMaybes)
import System.IO.Error
import Control.Monad (guard)

import Store.Blob as BlobStore
import Store.Ref as RefStore
import qualified Blob
import qualified Tree
import qualified Ref


logTag :: String
logTag = App.logTag ++ ".Plumbing"

hashObject :: B.ByteString -> Blob.Id
hashObject bytes = Blob.createId (Blob.Bytes bytes)

writeTree :: (BlobStore a) => FilePath -> a -> IO Blob.Id
writeTree path store = do
  tree <- createTree path store
  treeBlob <- saveTree tree store
  let (Blob.Blob blobId _) = treeBlob
  return blobId

setRef :: (RefStore a) => String -> Blob.Id -> a -> IO ()
setRef name value store = do
  let ref = Ref.create (Ref.createId name) value
  evalStateT (RefStore.set ref) store
  return ()

createTree :: (BlobStore a) => FilePath -> a -> IO Tree.Tree
createTree path store = do
  names <- listDirectory path
  entries <- mapM toEntry names
  return $ Tree.create (catMaybes entries)
  where
    toEntry name = createTreeEntry path name store

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = do
    names <- getDirectoryContents path
    return $ filter (`notElem` [".", ".."]) names

createTreeEntry :: (BlobStore a) => FilePath -> FilePath -> a -> IO (Maybe Tree.Entry)
createTreeEntry parentPath name store = do
    isDir <- doesDirectoryExist fullpath
    case isDir of
        True -> createDirEntry
        False -> createFileEntry
    where
        fullpath = parentPath </> name
        createDirEntry = do
            blobId <- writeTree fullpath store
            return . Just $ Tree.createEntry name 0 blobId
        createFileEntry = do
            result <- tryJust (guard . isDoesNotExistError) (blobFromPath fullpath)
            case result of
                Left e -> do
                    noticeM logTag ("Problem reading file: " ++ fullpath ++", exception: " ++ (show e))
                    return Nothing
                Right blob -> do
                    evalStateT (BlobStore.put blob) store
                    let (Blob.Blob blobId _) = blob
                    return . Just $ Tree.createEntry name 0 blobId


blobFromPath :: FilePath -> IO Blob.Blob
blobFromPath path = do
  contents <- B.readFile path
  return $ Blob.create contents

saveTree :: (BlobStore a) => Tree.Tree -> a -> IO Blob.Blob
saveTree tree store = do
  let treeBlob = Tree.toBlob tree
  evalStateT (BlobStore.put treeBlob) store
  return treeBlob
