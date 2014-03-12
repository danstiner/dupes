
module Store.Flat (
    createStore
  , Store
) where

import qualified Blob
import qualified Store.Blob

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import System.Directory as Directory
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)
import System.FilePath ( (</>) )

newtype Store = Store FilePath

pathPrefixSize :: Int
pathPrefixSize = 2

createStore :: FilePath -> Store
createStore path = Store path

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

instance Store.Blob.BlobStore Store where
	get key = do
		(Store storePath) <- State.get
		v <- lift $ L.readFile $ buildFilepath storePath key
		return $ Just (Blob.recreateLazy key v)

	put (Blob.Blob key val) = do
		(Store storePath) <- State.get
		let (parentDir, name) = buildFilepathParts storePath key
		let path = parentDir </> name
		lift $ Directory.createDirectoryIfMissing True parentDir
		lift $ writeToFile path val
		where
			writeToFile path (Blob.Bytes bytes) = B.writeFile path bytes
			writeToFile path (Blob.LazyBytes bytes) = L.writeFile path bytes

	delete key = do
		(Store storePath) <- State.get
		lift $ Directory.removeFile $ buildFilepath storePath key
