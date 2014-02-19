
-- Content addressed blobs
module Blob (
    Blob(Blob)
  , Id
  , Data
  , create
  , toString
) where

import Data.ByteString.Char8 as C
import Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA3 as SHA3

type HashDigest = ByteString

hashLength :: Int
hashLength = 256

data Id = Sha3 HashDigest deriving (Eq, Show, Ord)
type Data = ByteString

data Blob = Blob Id Data deriving (Show, Eq)

create :: ByteString -> Blob
create s = Blob (Sha3 $ SHA3.hash hashLength s) s

toString :: Id -> String
toString (Sha3 hash) =
  C.unpack $ Base16.encode hash
