module Tests.Store.Mem (tests) where

import Tests.Dupes (storeOpContract)
import Store.Mem

import Test.Framework

tests :: [Test]
tests = storeOpContract (return . evalStoreOp)
