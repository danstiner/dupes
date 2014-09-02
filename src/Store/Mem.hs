module Store.Mem (
    runStoreOp
  , evalStoreOp
) where

import Dupes

import Control.Monad.Free
import Control.Monad.State.Lazy
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude
import qualified Data.Map.Strict as Map

type DupeStore = (DupePathsStore, DupeBucketStore)
type DupePathsStore = Map PathKey PathKey
type DupeBucketStore = Map BucketKey (Set PathKey)

evalStoreOp :: StoreOp r -> r
evalStoreOp ops = evalState (runStoreOp ops) (Map.empty, Map.empty)

runStoreOp :: StoreOp r -> State DupeStore r
runStoreOp (Pure r) = return r
runStoreOp (Free (GetOp key f)) = get >>= runStoreOp . f . (Map.lookup key) . fst
runStoreOp (Free (PutOp path key t)) = do
  (paths, buckets) <- get
  put ((Map.insert path path paths), (Map.insertWith Set.union key (Set.singleton path) buckets))
  runStoreOp t
runStoreOp (Free (RmOp key t)) = do
  (paths, buckets) <- get
  put ((Map.delete key paths), buckets)
  runStoreOp t
runStoreOp (Free (ListOp _ f)) = get >>= runStoreOp . f . Map.keys . fst
runStoreOp (Free (BucketsOp f)) = get >>= runStoreOp . f . (map toBucket) . Map.assocs . snd
  where
    toBucket (key, paths) = Bucket key paths
