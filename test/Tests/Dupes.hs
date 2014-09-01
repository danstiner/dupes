{-# LANGUAGE RankNTypes #-}

module Tests.Dupes (storeOpContract) where

import ContentIdentifier as CI
import Dupes

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


storeOpContract :: (forall r . StoreOp r -> IO r) -> [Test]
storeOpContract f = [
    test_PutGet f
  , test_PutList f
  , test_PutBuckets f
  ]

test_PutGet :: (forall r . StoreOp r -> IO r) -> Test
test_PutGet f = testCase "Put then get should give putted value" $ do
  f actions >>= assertEqual "Got value is put value" (Just testPathKey)
  where
    testPathKey = toPathKey "test_path"
    actions = do
      putOp testPathKey
      getOp testPathKey

test_PutList :: (forall r . StoreOp r -> IO r) -> Test
test_PutList f = testCase "List after put should show one entry for put" $ do
  f actions >>= assertEqual "List after put shows value" [testPathKey]
  where
    testPathKey = toPathKey "test_path"
    actions = do
      putOp testPathKey
      listOp testPathKey

test_PutBuckets :: (forall r . StoreOp r -> IO r) -> Test
test_PutBuckets f = testCase "Bucket listing after put should show entry for put" $ do
  f actions >>= assertEqual "Buckets after put show value" [(Bucket key [pathKey])]
  where
    pathKey = toPathKey "test_path"
    key = CI.nil
    actions = do
      putOp pathKey
      bucketsOp
