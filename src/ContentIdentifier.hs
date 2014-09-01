{-# LANGUAGE DeriveGeneric #-}

module ContentIdentifier (
    create
  , createLazy
  , createNil
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
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Text.Read as R
import qualified Text.ParserCombinators.ReadP as ReadP
import Data.Char (isHexDigit)

data Algro = CRC32 | MD5 | SHA3_256 deriving (Generic)
type Digest = B.ByteString
type HashSize = Int
data Id = CRC32_Id Digest | MD5_Id Digest | SHA3 HashSize Digest | Nil deriving (Eq, Ord, Generic)

type IdString = String
type Type = Algro
type Value = Digest

sha3_256HashLength :: HashSize
sha3_256HashLength = 256

create :: Algro -> B.ByteString -> Id
create = hash

createLazy :: Algro -> L.ByteString -> Id
createLazy = hashLazy

createNil :: Id
createNil = Nil

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
  show (Nil) = "nil-"

instance Read Id where
  readPrec = R.lift $ do
    hex <- ReadP.munch1 isHexDigit
    return $ createIdFromHex hex

instance Serialize Algro where
  put CRC32    = put (1 :: Word8)
  put MD5      = put (2 :: Word8)
  put SHA3_256 = put (3 :: Word8)
  get = do
    t <- get :: Get Word8
    case t of
      1 -> return CRC32
      2 -> return MD5
      3 -> return SHA3_256
      _ -> fail "Unrecognized"

instance Serialize Id where
  put (MD5_Id   h) = put MD5      >> put h
  put (CRC32_Id h) = put CRC32    >> put h
  put (SHA3 256 h) = put SHA3_256 >> put h
  put (SHA3 i _) = fail ("Unsuppored SHA3 size: " ++ (show i))
  put (Nil) = fail ("Cannot serialize nil conetent identifier")

  get = do
    a <- get :: Get Algro
    case a of
      CRC32 -> get >>= return . CRC32_Id
      MD5   -> get >>= return . MD5_Id
      SHA3_256 -> get >>= return . SHA3 256

instance NFData Id where rnf = genericRnf
