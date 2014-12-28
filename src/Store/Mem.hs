module Store.Mem (
    runStoreOp
  , evalStoreOp
) where

import           Dupes

import           Control.Monad.Free
import           Control.Monad.State.Lazy
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Prelude

data DupeStore = DupeStore { pathStore :: DupePathsStore, bucketStore :: DupeBucketStore }
type DupePathsStore = Map PathKey BucketKey
type DupeBucketStore = Map BucketKey (Set PathKey)

evalStoreOp :: StoreOp r -> r
evalStoreOp ops = evalState (runStoreOp ops) (DupeStore Map.empty Map.empty)

runStoreOp :: StoreOp r -> State DupeStore r
runStoreOp (Pure r) = return r
runStoreOp (Free (GetOp key f)) = runStoreOp . f . Map.lookup key . pathStore =<< get
runStoreOp (Free (PutOp path key t)) = do
  (DupeStore paths buckets) <- get
  put (DupeStore (Map.insert path key paths) (Map.insertWith Set.union key (Set.singleton path) buckets))
  runStoreOp t
runStoreOp (Free (RmOp key t)) = do
  (DupeStore paths buckets) <- get
  put (DupeStore (Map.delete key paths) buckets)
  runStoreOp t
runStoreOp (Free (ListOp _ f)) = runStoreOp . f . Map.keys . pathStore =<< get
runStoreOp (Free (BucketsOp f)) = runStoreOp . f . map toBucket . Map.assocs . bucketStore =<< get
runStoreOp (Free (DupesOp f)) = runStoreOp . f . filter isDupe . map toBucket . Map.assocs . bucketStore =<< get
  where
    isDupe (Bucket _ paths) = Set.size paths > 1

toBucket :: (BucketKey, Set PathKey) -> Bucket
toBucket (key, paths) = Bucket key paths
