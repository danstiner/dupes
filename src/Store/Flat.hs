
module Store.Flat (
    createStore
  , Store
) where

import Store.Blob
import qualified Blob
import qualified App

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import System.Directory as Directory
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)
import System.FilePath ( (</>) )
import System.Log.Logger

newtype Store = Store FilePath

pathPrefixSize :: Int
pathPrefixSize = 2

logTag :: String
logTag = App.logTag ++ ".Store.Flat"

createStore :: FilePath -> Store
createStore = Store 
buildFilepath :: FilePath -> Blob.Id -> FilePath
buildFilepath path key =
  parentDir </> name
  where
    (parentDir, name) = buildFilepathParts path key

buildFilepathParts :: FilePath -> Blob.Id -> (FilePath,FilePath)
buildFilepathParts path key =
  (path </> "objects" </> prefix, postfix)
  where
    (prefix, postfix) = splitAt pathPrefixSize keystr
    keystr = Blob.toString key

instance BlobStore Store where
    get key = do
        (Store storePath) <- State.get
        lift $ infoM logTag ("get: " ++ Blob.toString key)
        v <- lift $ L.readFile $ buildFilepath storePath key
        return $ Just (Blob.recreateLazy key v)

    put (Blob.Blob key val) = do
        (Store storePath) <- State.get
        let (parentDir, name) = buildFilepathParts storePath key
        let path = parentDir </> name
        lift $ do
            infoM logTag ("put: " ++ Blob.toString key)
            Directory.createDirectoryIfMissing True parentDir
            writeToFile path val
        where
            writeToFile path (Blob.Bytes bytes) = B.writeFile path bytes
            writeToFile path (Blob.LazyBytes bytes) = L.writeFile path bytes

    delete key = do
        (Store storePath) <- State.get
        lift $ do
            infoM logTag ("delete: " ++ Blob.toString key)
            Directory.removeFile $ buildFilepath storePath key
