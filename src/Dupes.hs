{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Dupes (
    DupesMonad (..)
  , BucketType
  , Bucket (..)
  , BucketKey
  , Entry (..)
  , Dupes
  , DupesT
  , Key
  , CI.Algro (..)
  , execDupesT
  , createBucketKey
  , createBucketKeyLazy
  , nilBucketKey
) where

import ContentIdentifier as CI

import Data.Functor.Identity
import Control.Monad.Trans
import Data.Binary as Binary
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B


data Entry = Entry FilePath deriving (Generic)

newtype DupesT m a = DupesT { runDupesT :: m a}
type Dupes a = DupesT Identity a

type Key = CI.Id
type BucketType = CI.Type
type BucketKey = Key
data Bucket = Bucket Key [Entry] deriving (Generic)

class (Monad m) => DupesMonad m where
  list :: FilePath -> DupesT m [FilePath]
  buckets :: CI.Type -> DupesT m [Bucket]
  add  :: FilePath -> Key -> DupesT m ()
  get  :: FilePath -> DupesT m [Key]
  remove :: FilePath -> DupesT m [Key]
  removeDir :: FilePath -> DupesT m [Key]

execDupesT :: (Monad m) => DupesT m a -> m a
execDupesT = runDupesT

createBucketKey :: BucketType -> B.ByteString -> BucketKey
createBucketKey = CI.create

createBucketKeyLazy :: BucketType -> L.ByteString -> BucketKey
createBucketKeyLazy = CI.createLazy

nilBucketKey :: BucketKey
nilBucketKey = CI.createNil

instance MonadTrans DupesT where
  lift = DupesT

instance Eq Entry where
  (Entry a) == (Entry b) = a == b

instance Binary.Binary Entry where
  put (Entry path) = do
    Binary.put path
  get = do
    path <- Binary.get
    return (Entry path)

instance (DupesMonad m) => Monad (DupesT m) where
  fail str = DupesT $ fail str
  return a = DupesT $ do return a
  m >>= k  = DupesT $ do
      a <- runDupesT m
      runDupesT (k a)
