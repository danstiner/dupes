{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

module Dupes (
    Bucket (..)
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
  , mergeOrderedStreams
) where

import ContentIdentifier as CI

import Data.Machine.Interleave
import Data.Machine hiding ( run )
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


mergeOrderedStreams :: (Ord a, Monad m) => SourceT m a -> SourceT m a -> SourceT m (MergedOperation a)
mergeOrderedStreams a b = capped <~ b
  where
    capped = capXM a w

    w :: (Monad m, Ord a) => MachineT m (MY a a) (MergedOperation a)
    w = repeatedly start

    start :: (Ord a) => PlanT (MY a a) (MergedOperation a) m ()
    start = do
      x <- awaits MaybeX
      case x of
        Just l -> right l
        Nothing -> awaits JustY >>= yield . RightOnly

    left :: (Ord a) => a -> PlanT (MY a a) (MergedOperation a) m ()
    left r = do
      x <- awaits MaybeX
      case x of
        Just l -> mergeStep l r
        Nothing -> yield (RightOnly r) >> awaits JustY >>= yield . RightOnly

    right :: (Ord a) => a -> PlanT (MY a a) (MergedOperation a) m ()
    right l = do
      y <- awaits MaybeY
      case y of
        Just r -> mergeStep l r
        Nothing -> yield (LeftOnly l) >> awaits JustX >>= yield . LeftOnly

    mergeStep :: (Ord a) => a -> a -> PlanT (MY a a) (MergedOperation a) m ()
    mergeStep l r =
      if l == r
        then yield (Both l)
        else if l < r
          then do
            yield (LeftOnly l)
            left r
          else do
            yield (RightOnly r)
            right l


data Entry = Entry FilePath deriving (Generic)

newtype DupesT m a = DupesT { runDupesT :: m a}
type Dupes a = DupesT Identity a

type Key = CI.Id
type BucketType = CI.Type
type BucketKey = Key
data Bucket = Bucket Key [Entry] deriving (Generic)


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

