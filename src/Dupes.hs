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
  , Key
  , CI.Algro (..)
  , execDupes
  , createBucketKey
  , createBucketKeyLazy
) where

import ContentIdentifier as CI

import Control.Monad.Trans
import Data.Binary as Binary
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B


data Entry = Entry FilePath deriving (Generic)

newtype Dupes m a = Dupes { runDupes :: m a}

type Key = CI.Id
type BucketType = CI.Type
type BucketKey = Key
data Bucket = Bucket Key [Entry] deriving (Generic)

class (Monad m) => DupesMonad m where
  list :: CI.Type -> Dupes m [Bucket]
  add  :: FilePath -> Key -> Dupes m ()
  get  :: FilePath -> Dupes m [Key]
  remove :: FilePath -> Dupes m [Key]

execDupes :: (Monad m) => Dupes m a -> m a
execDupes = runDupes

createBucketKey :: BucketType -> B.ByteString -> BucketKey
createBucketKey = CI.create

createBucketKeyLazy :: BucketType -> L.ByteString -> BucketKey
createBucketKeyLazy = CI.createLazy

instance MonadTrans Dupes where
  lift = Dupes

instance Eq Entry where
  (Entry a) == (Entry b) = a == b

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
