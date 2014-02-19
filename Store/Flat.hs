

module Store.Flat (
    createStore
  , get
  , put
  , Store.Flat.delete
  , Store
) where

import qualified Blob

import qualified Data.ByteString as B
import System.Directory (removeFile)

data Store = Store FilePath

createStore :: FilePath -> Store
createStore path = Store path

buildFilepath :: FilePath -> Blob.Id -> FilePath
buildFilepath path key =
  path ++ "/" ++ (Blob.toString key)

get :: Blob.Id -> Store -> IO Blob.Data
get key (Store path) =
  B.readFile (buildFilepath path key)

put :: Blob.Blob -> Store -> IO ()
put (Blob.Blob key val) (Store path) =
  B.writeFile (buildFilepath path key) val

delete :: Blob.Id -> Store -> IO ()
delete key (Store path) =
  removeFile (buildFilepath path key)

