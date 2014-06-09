{-# LANGUAGE DeriveGeneric #-}

module ContentIdentifier (
  create
  , createLazy
  , Algro (..)
  , Type
  , Id
  , Value
    )
where

import Data.Binary as Binary
import Data.ByteString.Lazy.Builder
import Data.Digest.CRC32
import GHC.Generics (Generic)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Text.Read as R
import qualified Text.ParserCombinators.ReadP as ReadP
import Data.Char (isHexDigit)

data Algro = CRC32 | MD5 | SHA3_256 deriving (Generic)
type Digest = B.ByteString
type HashSize = Int
data Id = CRC32_Id Digest | MD5_Id Digest | SHA3 HashSize Digest deriving (Eq, Ord, Generic)

type IdString = String
type Type = Algro
type Value = Digest

sha3_256HashLength :: HashSize
sha3_256HashLength = 256

create :: Algro -> B.ByteString -> Id
create = hash

createLazy :: Algro -> L.ByteString -> Id
createLazy = hashLazy

hashLazy :: Algro -> L.ByteString -> Id
hashLazy MD5 = MD5_Id . MD5.hashlazy
hashLazy SHA3_256 = SHA3 len . (SHA3.hashlazy len) where len = sha3_256HashLength
hashLazy CRC32 = CRC32_Id . L.toStrict . toLazyByteString . word32BE . crc32

hash :: Algro -> B.ByteString -> Id
hash MD5 = MD5_Id . MD5.hash
hash SHA3_256 = SHA3 len . (SHA3.hash len) where len = sha3_256HashLength
hash CRC32 = CRC32_Id . L.toStrict . toLazyByteString . word32BE . crc32

createIdFromHex :: IdString -> Id
createIdFromHex s = 
  let (x, _) = Base16.decode (C.pack s) in
  SHA3 256 x

instance Show Id where
  show (CRC32_Id digest) = "crc32-" ++ (C.unpack $! Base16.encode digest)
  show (MD5_Id digest) = "md5-" ++ (C.unpack $! Base16.encode digest)
  show (SHA3 len digest) = "sha3_" ++ (show len) ++ "-" ++ (C.unpack $! Base16.encode digest)

instance Read Id where
  readPrec = lift $ do
    hex <- ReadP.munch1 isHexDigit
    return $ createIdFromHex hex

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
  put (MD5_Id d) = do
    Binary.put MD5
    Binary.put d
  put (CRC32_Id d) = do
    Binary.put CRC32
    Binary.put d

  get = do
    a <- Binary.get :: Get Algro
    case a of
      CRC32 -> Binary.get >>= return . CRC32_Id
      MD5   -> Binary.get >>= return . MD5_Id
