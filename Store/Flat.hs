
module Store.Flat (
    createStore
  , get
  , put
  , delete
  , Store
) where

import qualified Blob

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import System.Directory (removeFile)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

newtype Store = Store FilePath

type StoreState = State.StateT Store IO

createStore :: FilePath -> Store
createStore path = Store path

buildFilepath :: FilePath -> Blob.Id -> FilePath
buildFilepath path key =
  path ++ "/objects/" ++ (Blob.toString key)


get :: Blob.Id -> MaybeT StoreState Blob.Blob
get key = do
	(Store storePath) <- lift $ State.get
	b <- lift $ lift $ L.readFile $ buildFilepath storePath key
	return (Blob.recreateLazy key b)

put :: Blob.Blob -> StoreState ()
put (Blob.Blob key val) = do
	(Store storePath) <- State.get
	let path = buildFilepath storePath key
	lift $ writeFile path val
	where
		writeFile path (Blob.Bytes bytes) = B.writeFile path bytes
		writeFile path (Blob.LazyBytes bytes) = L.writeFile path bytes

delete :: Blob.Id -> StoreState ()
delete key = do
	(Store storePath) <- State.get
	lift $ removeFile $ buildFilepath storePath key
