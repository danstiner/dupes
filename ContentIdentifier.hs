{-# LANGUAGE DeriveGeneric #-}

module ContentIdentifier (
	create
  , createLazy
  , Algro (..)
  ,	Type
  , Id
  , Value
    )
where

import Data.ByteString.Lazy.Builder
import Data.Digest.CRC32
import GHC.Generics (Generic)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Binary as Binary

data Algro = CRC32 | MD5 | SHA3_256 deriving (Generic)
type Digest = B.ByteString
data Id = Id Algro Digest deriving (Generic)

type Type = Algro
type Value = Digest

sha3_256HashLength :: Int
sha3_256HashLength = 256

create :: Algro -> B.ByteString -> Id
create algro contents = Id algro $! (hash algro contents)

createLazy :: Algro -> L.ByteString -> Id
createLazy algro contents = Id algro $! (hashLazy algro contents)

hashLazy :: Algro -> L.ByteString -> Digest
hashLazy MD5 = MD5.hashlazy
hashLazy SHA3_256 = SHA3.hashlazy sha3_256HashLength
hashLazy CRC32 = L.toStrict . toLazyByteString . word32BE . crc32

hash :: Algro -> B.ByteString -> Digest
hash MD5 = MD5.hash
hash SHA3_256 = SHA3.hash sha3_256HashLength
hash CRC32 = L.toStrict . toLazyByteString . word32BE . crc32


instance Binary.Binary Algro where
  put CRC32    = Binary.put (1 :: Word8)
  put MD5      = Binary.put (2 :: Word8)
  put SHA3_256 = Binary.put (3 :: Word8)
  get = do
    t <- Binary.get :: Get Word8
    case t of
      1 -> return CRC32
      2 -> return MD5
      3 -> return SHA3_256
      _ -> fail "Unrecognized"

instance Binary.Binary Id where
  put (Id a d) = do
    Binary.put a
    Binary.put d
  get = do
    a <- Binary.get
    d <- Binary.get
    return (Id a d)
