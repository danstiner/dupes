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
  , createBucketKey
  , createBucketKeyLazy
  , execDupesT
  , nilBucketKey
  , MergedOperation (..)
  , PathKey (..)
  , combine
  , toPathKey
  , mergeOrderedStreams
  , mergeOrderedStreamsWye
  , StoreOp
  , StoreOpF (..)
  , getOp, putOp, rmOp, listOp, bucketsOp, dupesOp
) where

import ContentIdentifier as CI

import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Machine hiding ( run )
import Data.Machine.Interleave
import Data.Serialize
import Data.Set (Set)
import GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data MergedOperation a = LeftOnly a | RightOnly a | Both a deriving (Show, Eq)

type FilePathByteString = C.ByteString

newtype PathKey = PathKey { unPathKey :: FilePathByteString } deriving (Eq, Ord, Show)

instance Serialize PathKey where
  put = put . unPathKey
  get = liftM PathKey get

toPathKey :: FilePath -> PathKey
toPathKey = PathKey . C.pack

combine :: (Ord a, Eq a) => [a] -> [a] -> [MergedOperation a]
combine [] [] = []
combine xs [] = map LeftOnly xs
combine [] ys = map RightOnly ys
combine xa@(x:xs) ya@(y:ys)
  | x == y = Both x : combine xs ys
  | x <  y = LeftOnly x : combine xs ya
  | otherwise = RightOnly y : combine xa ys

mergeOrderedStreams :: (Ord a, Monad m) => SourceT m a -> SourceT m a -> SourceT m (MergedOperation a)
mergeOrderedStreams a b = capped <~ b
  where
    capped = capXM a mergeOrderedStreamsWye

mergeOrderedStreamsWye :: (Monad m, Ord a) => MachineT m (MY a a) (MergedOperation a)
mergeOrderedStreamsWye= repeatedly start
  where
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

newtype DupesT m a = DupesT { runDupesT :: m a}
type Dupes a = DupesT Identity a

type BucketType = CI.Type
type BucketKey = CI.Id
data Bucket = Bucket BucketKey (Set PathKey) deriving (Generic, Show, Eq)

execDupesT :: (Monad m) => DupesT m a -> m a
execDupesT = runDupesT

createBucketKey :: BucketType -> B.ByteString -> BucketKey
createBucketKey = CI.create

createBucketKeyLazy :: BucketType -> L.ByteString -> BucketKey
createBucketKeyLazy = CI.createLazy

nilBucketKey :: BucketKey
nilBucketKey = CI.nil

instance MonadTrans DupesT where
  lift = DupesT

data StoreOpF x = GetOp PathKey (Maybe PathKey -> x) | PutOp PathKey BucketKey x | RmOp PathKey x | ListOp PathKey ([PathKey] -> x) | BucketsOp ([Bucket] -> x) | DupesOp ([Bucket] -> x)

instance Functor StoreOpF where
  fmap f (GetOp path g) = GetOp path (f . g)
  fmap f (PutOp path key x) = PutOp path key (f x)
  fmap f (RmOp key x) = RmOp key (f x)
  fmap f (ListOp prefix g) = ListOp prefix (f . g)
  fmap f (BucketsOp g) = BucketsOp (f . g)
  fmap f (DupesOp g) = DupesOp (f . g)

type StoreOp = Free StoreOpF

getOp :: PathKey -> StoreOp (Maybe PathKey)
getOp path = liftF $ GetOp path id

putOp :: PathKey -> BucketKey -> StoreOp ()
putOp path key = liftF $ PutOp path key ()

rmOp :: PathKey -> StoreOp ()
rmOp key = liftF $ RmOp key ()

listOp :: PathKey -> StoreOp [PathKey]
listOp prefix = liftF $ ListOp prefix id

bucketsOp :: StoreOp [Bucket]
bucketsOp = liftF $ BucketsOp id

dupesOp :: StoreOp [Bucket]
dupesOp = liftF $ DupesOp id
