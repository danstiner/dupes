module Tests.Store.LevelDB (tests) where

import Store.LevelDB

import Data.Either (rights)
import Data.List
import Data.List.Ordered
import Data.Serialize

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests = [ (testProperty "Serialized DupesPathLevelKey order as normal" prop_serializedSimplePathKeysOrder) ]

prop_orderedSerialization :: (Serialize a, Ord a) => [a] -> Bool
prop_orderedSerialization = isSorted . endecode
  where
    endecode :: (Serialize b) => [b] -> [b]
    endecode xs = rights . map decode . sort . map encode $ xs

prop_serializedSimplePathKeysOrder :: [DupesPathLevelKey] -> Bool
prop_serializedSimplePathKeysOrder = prop_orderedSerialization
