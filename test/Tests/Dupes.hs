{-# LANGUAGE RankNTypes #-}

module Tests.Dupes (storeOpContract) where

import           ContentIdentifier              as CI
import           Dupes

import qualified Data.Set                       as Set

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)

storeOpContract :: (forall r . StoreOp r -> IO r) -> [Test]
storeOpContract f = [
    testPutGet f
  , testPutList f
  , testPutSingleNoDupeBuckets f
  , testPutDupesVerifyDupeBucket f
  ]

testPutGet :: (forall r . StoreOp r -> IO r) -> Test
testPutGet f = testCase "Put then get path" $
  f actions >>= assertEqual "Got value is put value" (Just bucketKey)
  where
    pathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp pathKey bucketKey
      getOp pathKey

testPutList :: (forall r . StoreOp r -> IO r) -> Test
testPutList f = testCase "List after put should show one entry for put" $
  f actions >>= assertEqual "List after put shows value" [testPathKey]
  where
    testPathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp testPathKey bucketKey
      listOp testPathKey

testPutSingleNoDupeBuckets :: (forall r . StoreOp r -> IO r) -> Test
testPutSingleNoDupeBuckets f = testCase "Dupes list after put should have no entries" $
  f actions >>= assertEqual "No buckets after put" []
  where
    pathKey = toPathKey "test_path"
    bucketKey = CI.nil
    actions = do
      putOp pathKey bucketKey
      dupesOp

testPutDupesVerifyDupeBucket :: (forall r . StoreOp r -> IO r) -> Test
testPutDupesVerifyDupeBucket f = testCase "Bucket list after duplicate puts should have single entry" $
  f actions >>= assertEqual "Single bucket" [Bucket bucketKey pathKeySet]
  where
    pathKey1 = toPathKey "test_path1"
    pathKey2 = toPathKey "test_path2"
    pathKeySet = Set.insert pathKey2 $ Set.singleton pathKey1
    bucketKey = CI.nil
    actions = do
      putOp pathKey1 bucketKey
      putOp pathKey2 bucketKey
      dupesOp
