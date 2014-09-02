module Store.Mem (
    runStoreOp
  , evalStoreOp
) where

import Dupes

import Control.Monad.Free
import Control.Monad.State.Lazy
import Data.Map.Strict hiding (map)
import Prelude hiding (lookup)

type DupeStore = (DupePathsStore, DupeBucketStore)
type DupePathsStore = Map PathKey PathKey
type DupeBucketStore = Map BucketKey [PathKey]

evalStoreOp :: StoreOp r -> r
evalStoreOp ops = evalState (runStoreOp ops) (empty, empty)

runStoreOp :: StoreOp r -> State DupeStore r
runStoreOp (Pure r) = return r
runStoreOp (Free (GetOp key f)) = get >>= runStoreOp . f . (lookup key) . fst
runStoreOp (Free (PutOp path key t)) = do
  (paths, buckets) <- get
  put ((insert path path paths), (insert key [path] buckets))
  runStoreOp t
runStoreOp (Free (RmOp key t)) = do
  (paths, buckets) <- get
  put ((delete key paths), buckets)
  runStoreOp t
runStoreOp (Free (ListOp _ f)) = get >>= runStoreOp . f . keys . fst
runStoreOp (Free (BucketsOp f)) = get >>= runStoreOp . f . (map toBucket) . assocs . snd
  where
    toBucket (key, paths) = Bucket key paths
