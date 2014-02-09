

module InMem (
    createStore
  , get
  , put
  , InMem.delete
  , MemStore
) where

import qualified Blob

import Data.Map as Map

data MemStore = MemStore (Map Blob.Id Blob.Data) deriving (Show)

createStore :: MemStore
createStore = MemStore empty

get :: Blob.Id -> MemStore -> Maybe Blob.Data
get key (MemStore map) = Map.lookup key map

put :: Blob.Blob -> MemStore -> MemStore
put (Blob.Blob id dat) (MemStore map) =
  MemStore $ insert id dat map

delete :: Blob.Id -> MemStore -> MemStore
delete id (MemStore map) =
  MemStore $ Map.delete id map
