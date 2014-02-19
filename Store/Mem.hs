
module Store.Mem (
    createStore
  , get
  , put
  , Store.Mem.delete
  , MemStore
) where

import qualified Blob
import Data.Map as Map

newtype MemStore = MemStore (Map Blob.Id Blob.Data) deriving (Show)

createStore :: MemStore
createStore = MemStore empty

get :: Blob.Id -> MemStore -> Maybe Blob.Data
get i (MemStore m) = Map.lookup i m

put :: Blob.Blob -> MemStore -> MemStore
put (Blob.Blob i dat) (MemStore m) =
  MemStore $ insert i dat m

delete :: Blob.Id -> MemStore -> MemStore
delete i (MemStore m) =
  MemStore $ Map.delete i m
