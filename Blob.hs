
-- Content addressed blobs
module Blob (
    Blob(Blob)
  , Id
  , Data (..)
  , create
  , recreateLazy
  , toString
) where

import Data.Binary as Binary
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.SHA3 as SHA3

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

type HashDigest = B.ByteString

hashLength :: Int
hashLength = 256

data Id = Sha3 HashDigest deriving (Eq, Show, Ord)
data Data = Bytes B.ByteString | LazyBytes L.ByteString deriving (Show, Eq)

data Blob = Blob Id Data deriving (Show, Eq)

instance Binary.Binary Id where
	put (Sha3 hash) = do
		Binary.put hash
	get = do
		hash <- Binary.get
		return $ Sha3 hash

create :: B.ByteString -> Blob
create s = Blob (Sha3 $ SHA3.hash hashLength s) (Bytes s)

recreateLazy :: Blob.Id -> L.ByteString -> Blob
recreateLazy i d = Blob i (LazyBytes d)

toString :: Id -> String
toString (Sha3 hash) =
  C.unpack $ Base16.encode hash

