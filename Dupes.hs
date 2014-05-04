{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Dupes (
    DupesMonad (..)
  , BucketType (..)
  , Bucket (..)
  , BucketKey (..)
  , Entry (..)
  , Dupes (..)
  , execDupes
  , createBucketKey
  , createBucketKeyLazy
) where

import Control.Monad.Trans
import Data.Binary as Binary
import Data.ByteString
import Data.ByteString.Lazy.Builder
import Data.Digest.CRC32
import GHC.Generics (Generic)
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

--newtype FileMode = FileMode Int

data Entry = Entry FilePath deriving (Generic)

newtype Dupes m a = Dupes { runDupes :: m a}

data BucketType = CRC32 | MD5 | SHA3 deriving (Generic)
type BucketName = ByteString
data BucketKey = BucketKey BucketType BucketName deriving (Generic)
data Bucket = Bucket BucketKey [Entry] deriving (Generic)

hashLength :: Int
hashLength = 256

class (Monad m) => DupesMonad m where
  list :: BucketType -> Dupes m [Bucket]
  add :: FilePath -> BucketKey -> Dupes m ()
  get :: FilePath -> Dupes m [BucketKey]
  rm :: FilePath -> Dupes m [BucketKey]

execDupes :: (Monad m) => Dupes m a -> m a
execDupes = runDupes

createBucketKey :: BucketType -> B.ByteString -> BucketKey
createBucketKey bType contents = BucketKey bType $! (createBucketName bType contents)

createBucketKeyLazy :: BucketType -> L.ByteString -> BucketKey
createBucketKeyLazy bType contents = BucketKey bType $! (createBucketNameLazy bType contents)


createBucketNameLazy :: BucketType -> L.ByteString -> BucketName
createBucketNameLazy MD5 = MD5.hashlazy
createBucketNameLazy SHA3 = SHA3.hashlazy hashLength
createBucketNameLazy CRC32 = L.toStrict . toLazyByteString . word32BE . crc32

createBucketName :: BucketType -> B.ByteString -> BucketName
createBucketName MD5 = MD5.hash
createBucketName SHA3 = SHA3.hash hashLength
createBucketName CRC32 = L.toStrict . toLazyByteString . word32BE . crc32


instance MonadTrans Dupes where
  lift = Dupes

instance Eq Entry where
  (Entry a) == (Entry b) = a == b

instance Binary.Binary BucketType where
  put CRC32 = Binary.put (1 :: Word8)
  put MD5   = Binary.put (2 :: Word8)
  put SHA3  = Binary.put (3 :: Word8)
  get = do
    t <- Binary.get :: Get Word8
    case t of
      1 -> return CRC32
      2 -> return MD5
      3 -> return SHA3
      _ -> fail "Unrecognized"

instance Binary.Binary BucketKey where
  put (BucketKey t v) = do
    Binary.put t
    Binary.put v
  get = do
    t <- Binary.get
    v <- Binary.get
    return (BucketKey t v)

instance Binary.Binary Entry where
  put (Entry path) = do
    Binary.put path
  get = do
    path <- Binary.get
    return (Entry path)

instance (DupesMonad m) => Monad (Dupes m) where
  fail str = Dupes $ fail str
  return a = Dupes $ do return a
  m >>= k  = Dupes $ do
      a <- runDupes m
      runDupes (k a)
