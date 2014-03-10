
module Store.Flat (
    createStore
  , Store
) where

import qualified Blob
import qualified Store.Blob

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import System.Directory (removeFile)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)

newtype Store = Store FilePath

createStore :: FilePath -> Store
createStore path = Store path

buildFilepath :: FilePath -> Blob.Id -> FilePath
buildFilepath path key =
  path ++ "/objects/" ++ (Blob.toString key)

instance Store.Blob.BlobStore Store where
	get key = do
		(Store storePath) <- State.get
		v <- lift $ L.readFile $ buildFilepath storePath key
		return $ Just (Blob.recreateLazy key v)

	put (Blob.Blob key val) = do
		(Store storePath) <- State.get
		let path = buildFilepath storePath key
		lift $ writeToFile path val
		where
			writeToFile path (Blob.Bytes bytes) = B.writeFile path bytes
			writeToFile path (Blob.LazyBytes bytes) = L.writeFile path bytes

	delete key = do
		(Store storePath) <- State.get
		lift $ removeFile $ buildFilepath storePath key
