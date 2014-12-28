module Tests.Store.Mem (tests) where

import           Store.Mem
import           Tests.Dupes    (storeOpContract)

import           Test.Framework

tests :: [Test]
tests = storeOpContract (return . evalStoreOp)
