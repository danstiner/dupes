{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Dupes (
    DupesMonad (..)
  , Bucket (..)
  , BucketKey
  , BucketType
  , CI.Algro (..)
  , Dupes
  , DupesT
  , Entry (..)
  , Key
  , createBucketKey
  , createBucketKeyLazy
  , execDupesT
  , nilBucketKey
  , MergedOperation (..)
  , PathKey
  , combine
  , toPathKey
) where

import ContentIdentifier as CI

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 as C (pack, unpack)
import Data.Functor.Identity
import Data.Serialize
import GHC.Generics (Generic)
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Test.QuickCheck

data MergedOperation a = LeftOnly a | RightOnly a | Both a deriving (Show, Eq)

newtype PathKey = PathKey { unPathKey :: FilePath } deriving (Eq, Ord, Show)

instance Serialize PathKey where
  put = putByteString . C.pack . unPathKey
  get = liftM PathKey (fmap C.unpack $ remaining >>= getByteString)

instance Arbitrary PathKey where
  arbitrary = PathKey <$> arbitrary

toPathKey :: FilePath -> PathKey
toPathKey = PathKey

combine :: (Ord a, Eq a) => [a] -> [a] -> [MergedOperation a]
combine [] [] = []
combine xs [] = map LeftOnly xs
combine [] ys = map RightOnly ys
combine xa@(x:xs) ya@(y:ys)
  | x == y = Both x : combine xs ys
  | x <  y = LeftOnly x : combine xs ya
  | x >  y = RightOnly y : combine xa ys

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
