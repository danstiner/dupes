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
  , test_PutBuckets f
  , test_PutDupes_VerifyBucket f
  ]

test_PutGet :: (forall r . StoreOp r -> IO r) -> Test
test_PutGet f = testCase "Put then get path" $ do
  f actions >>= assertEqual "Got value is put value" (Just testPathKey)
  where
    testPathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp testPathKey bucketKey
      getOp testPathKey

test_PutList :: (forall r . StoreOp r -> IO r) -> Test
test_PutList f = testCase "List after put should show one entry for put" $ do
  f actions >>= assertEqual "List after put shows value" [testPathKey]
  where
    testPathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp testPathKey bucketKey
      listOp testPathKey

test_PutBuckets :: (forall r . StoreOp r -> IO r) -> Test
test_PutBuckets f = testCase "Bucket list after put should have single entry" $ do
  f actions >>= assertEqual "Bucket after put show value" [(Bucket bucketKey (Set.singleton pathKey))]
  where
    pathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp pathKey bucketKey
      bucketsOp

test_PutDupes_VerifyBucket :: (forall r . StoreOp r -> IO r) -> Test
test_PutDupes_VerifyBucket f = testCase "Bucket list after duplicate puts should have single entry" $ do
  f actions >>= assertEqual "Single bucket" [(Bucket bucketKey (Set.insert pathKey2 $ Set.singleton pathKey1))]
  where
    pathKey1 = toPathKey "test_path1"
    pathKey2 = toPathKey "test_path2"
    bucketKey = CI.nil
    actions = do
      putOp pathKey1 bucketKey
      putOp pathKey2 bucketKey
      bucketsOp
