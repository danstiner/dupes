
-- Content addressed blobs
module Blob (
    Blob(Blob)
  , BlobId
  , BlobData
  , createBlob
) where

data BlobId = IntBlobId Int deriving (Eq, Show, Ord)
type BlobData = String

data Blob = Blob BlobId BlobData deriving (Show, Eq)

createBlob :: String -> Blob
createBlob s = Blob (IntBlobId 0) s

--get :: BlobId -> (Result, Maybe Blob)
--store :: Blob -> (Result, Maybe BlobId)
--delete :: BlobId -> Result

