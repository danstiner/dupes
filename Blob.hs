
-- Content addressed blobs
module Blob (
    Blob(Blob)
  , BlobId
  , BlobData
  , createBlob
) where

import Data.ByteString (ByteString)
import Crypto.Hash.SHA3 (hash, hashlazy)

type HashDigest = ByteString

hashLength = 256

data BlobId = Sha3BlobId HashDigest deriving (Eq, Show, Ord)
type BlobData = ByteString

data Blob = Blob BlobId BlobData deriving (Show, Eq)

createBlob :: ByteString -> Blob
createBlob s = Blob (Sha3BlobId $ hash hashLength s) s

