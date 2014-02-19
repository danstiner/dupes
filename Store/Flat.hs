

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
buildFilepath path id =
  path ++ "/" ++ (Blob.toString id)

get :: Blob.Id -> Store -> IO Blob.Data
get id (Store path) = 
  B.readFile (buildFilepath path id)

put :: Blob.Blob -> Store -> IO ()
put (Blob.Blob id val) (Store path) =
  B.writeFile (buildFilepath path id) val

delete :: Blob.Id -> Store -> IO ()
delete id (Store path) =
  removeFile (buildFilepath path id)

