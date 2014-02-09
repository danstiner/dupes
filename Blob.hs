
-- Content addressed blobs
module Blob (
    Blob(Blob)
  , Id
  , Data
  , create
) where

import Data.ByteString (ByteString)
import qualified Crypto.Hash.SHA3 as SHA3

type HashDigest = ByteString

hashLength = 256

data Id = Sha3 HashDigest deriving (Eq, Show, Ord)
type Data = ByteString

data Blob = Blob Id Data deriving (Show, Eq)

create :: ByteString -> Blob
create s = Blob (Sha3 $ SHA3.hash hashLength s) s

