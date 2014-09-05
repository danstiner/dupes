{-# LANGUAGE DeriveGeneric #-}

module ContentIdentifier (
    create
  , createLazy
  , nil
  , Algro (..)
  , Type
  , Id
  , Value
    )
where

import Data.Word
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)
import Data.ByteString.Lazy.Builder
import Data.Serialize
import Data.Digest.CRC32
import GHC.Generics (Generic)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Text.Read as R
import qualified Text.ParserCombinators.ReadP as ReadP
import Data.Char (isHexDigit)

data Algro = CRC32 | MD5 | SHA1 | SHA3_256 | Nil deriving (Generic)
type Digest = B.ByteString
type HashSize = Int
data Id = CRC32_Id Digest | MD5_Id Digest | SHA1_Id Digest | SHA3 HashSize Digest | Nil_Id deriving (Eq, Ord, Generic)

type IdString = String
type Type = Algro
type Value = Digest

sha3_256HashLength :: HashSize
sha3_256HashLength = 256

create :: Algro -> B.ByteString -> Id
create = hash

createLazy :: Algro -> L.ByteString -> Id
createLazy = hashLazy

nil :: Id
nil = Nil_Id

hashLazy :: Algro -> L.ByteString -> Id
hashLazy MD5 = MD5_Id . MD5.hashlazy
hashLazy SHA1 = SHA1_Id . SHA1.hashlazy
hashLazy SHA3_256 = SHA3 len . (SHA3.hashlazy len) where len = sha3_256HashLength
hashLazy CRC32 = CRC32_Id . L.toStrict . toLazyByteString . word32BE . crc32
hashLazy Nil = undefined

hash :: Algro -> B.ByteString -> Id
hash MD5 = MD5_Id . MD5.hash
hash SHA1 = SHA1_Id . SHA1.hash
hash SHA3_256 = SHA3 len . (SHA3.hash len) where len = sha3_256HashLength
hash CRC32 = CRC32_Id . L.toStrict . toLazyByteString . word32BE . crc32
hash Nil = undefined

createIdFromHex :: IdString -> Id
createIdFromHex s = 
  let (x, _) = Base16.decode (C.pack s) in
  SHA3 256 x

instance Show Id where
  show (CRC32_Id digest) = "crc32-" ++ (C.unpack $! Base16.encode digest)
  show (MD5_Id digest) = "md5-" ++ (C.unpack $! Base16.encode digest)
  show (SHA1_Id digest) = "sha1-" ++ (C.unpack $! Base16.encode digest)
  show (SHA3 len digest) = "sha3_" ++ (show len) ++ "-" ++ (C.unpack $! Base16.encode digest)
  show (Nil_Id) = "nil-0"

instance Read Id where
  readPrec = R.lift $ do
    hex <- ReadP.munch1 isHexDigit
    return $ createIdFromHex hex

instance Serialize Algro where
  put Nil      = put (0 :: Word8)
  put CRC32    = put (1 :: Word8)
  put MD5      = put (2 :: Word8)
  put SHA3_256 = put (3 :: Word8)
  put SHA1     = put (4 :: Word8)
  get = do
    t <- get :: Get Word8
    case t of
      0 -> return Nil
      1 -> return CRC32
      2 -> return MD5
      3 -> return SHA3_256
      4 -> return SHA1
      _ -> fail "Unrecognized"

instance Serialize Id where
  put (MD5_Id   d) = put MD5   >> put d
  put (CRC32_Id d) = put CRC32 >> put d
  put (SHA1_Id  d) = put SHA1  >> put d
  put (SHA3 256 d) = put SHA3_256 >> put d
  put (SHA3 i _) = fail ("Unsuppored SHA3 size: " ++ (show i))
  put (Nil_Id) = put Nil

  get = do
    a <- get :: Get Algro
    case a of
      CRC32 -> get >>= return . CRC32_Id
      MD5   -> get >>= return . MD5_Id
      SHA1  -> get >>= return . SHA1_Id
      SHA3_256 -> get >>= return . SHA3 sha3_256HashLength
      Nil -> return Nil_Id

instance NFData Id where rnf = genericRnf
