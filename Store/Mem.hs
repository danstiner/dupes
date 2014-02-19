
module Store.Mem (
    createStore
  , get
  , put
  , delete
  , MemStore
) where

import qualified Blob
import qualified Data.Map as Map
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)

newtype MemStore = MemStore (Map.Map Blob.Id Blob.Data) deriving (Show)

type MemStateT = State.StateT MemStore
type MemState = State.State MemStore

createStore :: MemStore
createStore = MemStore Map.empty

getS :: Blob.Id -> MemStateT Maybe Blob.Data
getS i = do
	(MemStore m) <- State.get
	v <- lift $ Map.lookup i m
	return v

putS :: Blob.Blob -> MemState ()
putS (Blob.Blob i dat) = do
	(MemStore m) <- State.get
	let newStore = MemStore $ Map.insert i dat m
	State.put $ newStore

deleteS :: Blob.Id -> MemState ()
deleteS i = do
	(MemStore m) <- State.get
	let newStore = MemStore $ Map.delete i m
	State.put $ newStore

get :: Blob.Id -> MemStore -> Maybe Blob.Data
get i (MemStore m) = Map.lookup i m

put :: Blob.Blob -> MemStore -> MemStore
put (Blob.Blob i dat) (MemStore m) =
  MemStore $ Map.insert i dat m

delete :: Blob.Id -> MemStore -> MemStore
delete i (MemStore m) =
  MemStore $ Map.delete i m
