

module InMem (
    createStore
  , get
  , put
  , InMem.delete
) where

import Blob

import Data.Map as Map

data MemStore = MemStore (Map BlobId BlobData) deriving (Show)

createStore :: MemStore
createStore = MemStore empty

get :: BlobId -> MemStore -> Maybe BlobData
get key (MemStore map) = Map.lookup key map

put :: Blob -> MemStore -> MemStore
put (Blob id dat) (MemStore map) =
  MemStore $ insert id dat map

delete :: BlobId -> MemStore -> MemStore
delete id (MemStore map) =
  MemStore $ Map.delete id map
