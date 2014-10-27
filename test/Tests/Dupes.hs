{-# LANGUAGE RankNTypes #-}

module Tests.Dupes (storeOpContract) where

import ContentIdentifier as CI
import Dupes

import qualified Data.Set as Set

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

storeOpContract :: (forall r . StoreOp r -> IO r) -> [Test]
storeOpContract f = [
    test_PutGet f
  , test_PutList f
  , test_PutSingle_NoDupeBuckets f
  , test_PutDupes_VerifyDupeBucket f
  ]

test_PutGet :: (forall r . StoreOp r -> IO r) -> Test
test_PutGet f = testCase "Put then get path" $ do
  f actions >>= assertEqual "Got value is put value" (Just bucketKey)
  where
    pathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp pathKey bucketKey
      getOp pathKey

test_PutList :: (forall r . StoreOp r -> IO r) -> Test
test_PutList f = testCase "List after put should show one entry for put" $ do
  f actions >>= assertEqual "List after put shows value" [testPathKey]
  where
    testPathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp testPathKey bucketKey
      listOp testPathKey

test_PutSingle_NoDupeBuckets :: (forall r . StoreOp r -> IO r) -> Test
test_PutSingle_NoDupeBuckets f = testCase "Dupes list after put should have no entries" $ do
  f actions >>= assertEqual "No buckets after put" []
  where
    pathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp pathKey bucketKey
      dupesOp

test_PutDupes_VerifyDupeBucket :: (forall r . StoreOp r -> IO r) -> Test
test_PutDupes_VerifyDupeBucket f = testCase "Bucket list after duplicate puts should have single entry" $ do
  f actions >>= assertEqual "Single bucket" [(Bucket bucketKey pathKeySet)]
  where
    pathKey1 = toPathKey "test_path1"
    pathKey2 = toPathKey "test_path2"
    pathKeySet = (Set.insert pathKey2 $ Set.singleton pathKey1)
    bucketKey = CI.nil
    actions = do
      putOp pathKey1 bucketKey
      putOp pathKey2 bucketKey
      dupesOp
