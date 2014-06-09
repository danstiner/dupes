
module Store.Mem (
    createStore
  , MemStore
) where

import qualified Blob
import qualified Ref
import qualified Store.Blob
import qualified Store.Ref

import qualified Data.Map as Map
import qualified Control.Monad.Trans.State as State

newtype MemStore = MemStore (Map.Map Blob.Id Blob.Data) deriving (Show)
newtype MemRefStore = MemRefStore (Map.Map Ref.Id Blob.Id) deriving (Show)

createStore :: MemStore
createStore = MemStore Map.empty

instance Store.Blob.BlobStore MemStore where
	get key = do
		(MemStore m) <- State.get
		let v = Map.lookup key m
		case v of
			Just val -> return $ Just (Blob.Blob key val)
			Nothing -> return Nothing

	put (Blob.Blob key dat) = do
		(MemStore m) <- State.get
		let newStore = MemStore $ Map.insert key dat m
		State.put $ newStore

	delete key = do
		(MemStore m) <- State.get
		let newStore = MemStore $ Map.delete key m
		State.put $ newStore

instance Store.Ref.RefStore MemRefStore where
	read name = do
		(MemRefStore m) <- State.get
		let v = Map.lookup name m
		case v of
			Just val -> return $ Just (Ref.Ref name val)
			Nothing -> return Nothing

	set (Ref.Ref name ref) = do
		(MemRefStore m) <- State.get
		let newStore = MemRefStore $ Map.insert name ref m
		State.put $ newStore

	delete key = do
		(MemRefStore m) <- State.get
		let newStore = MemRefStore $ Map.delete key m
		State.put $ newStore