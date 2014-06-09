module Tests.Store.Mem (tests) where

import qualified Data.ByteString.Char8 as C
import Control.Monad.Trans.State (evalStateT)

import qualified Blob as Blob
import Store.Mem
import Store.Blob

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


tests :: [Test]
tests = [ testPutGet_ReturnsBlob
        , testPutDeleteGet_ReturnsNothing
        ]

testBlob :: Blob.Blob
testBlob = Blob.create $ C.pack "Test String"

testPutGet_ReturnsBlob :: Test
testPutGet_ReturnsBlob = testCase "Getting a put blob should succeed" $ do
  evalStateT actions store >>= assertEqual "Should retrieve stored blob" (Just blob)
  where
    store = createStore
    blob = testBlob
    (Blob.Blob blobId _) = blob
    actions = do
      put blob
      return =<< get blobId


testPutDeleteGet_ReturnsNothing :: Test
testPutDeleteGet_ReturnsNothing = testCase "Getting a deleted put blob should fail" $ do
  evalStateT actions store >>= assertEqual "Should retrieve nothing" Nothing
  where
    store = createStore
    blob = testBlob
    (Blob.Blob blobId _) = blob
    actions = do
      put blob
      delete blobId
      return =<< get blobId
