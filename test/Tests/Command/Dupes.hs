{-# LANGUAGE FlexibleInstances #-}

module Tests.Command.Dupes (tests) where

import Dupes

import Control.Monad
import Data.Either (rights)
import Data.List
import Data.List.Ordered
import Data.Serialize
import Data.Word

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

tests :: [Test]
tests = [ (testProperty "Rebuilding combined lists is identity" prop_combineAndRebuildIntList)
        , (testProperty "Encode decode PathKey is identity" prop_serializePathKey)
        , (testProperty "Encoded Words are orderable" prop_serializedNumbersOrderable)
        , (testProperty "Encoded PathKeys are orderable" prop_serializedPathKeysOrderable)
        , (testProperty "Rebuilding combined flattened DirTrees is identity" prop_combineAndRebuildFlattenedDirTrees)
        , (testProperty "Combine two DirTrees, one sorted while serialized" prop_SerializedDirTreeCombineDirTree)]

rebuild :: [MergedOperation a] -> ([a], [a])
rebuild = r
  where
    r [] = ([], [])
    r ((LeftOnly x) : xs) = left x (rebuild xs)
    r (RightOnly x : xs) = right x (rebuild xs)
    r (Both x : xs) = left x $ right x (rebuild xs)
    left x (xs, ys) = (x : xs, ys)
    right y (xs, ys) = (xs, y : ys)

instance (Arbitrary a) => Arbitrary (DirTree a) where
  arbitrary = sized $ \s -> frequency
     [(s, (liftM2 File arbitrary arbitrary)),
      (1, (liftM3 Dir arbitrary arbitrary arbitrary))]

instance Arbitrary PathKey where
  arbitrary = liftM2 PathKey arbitrary arbitrary

prop_combineAndRebuildIntList :: OrderedList Int -> OrderedList Int -> Bool
prop_combineAndRebuildIntList olx oly =
  (ident xs ys) == (xs, ys)
  where
    xs = getOrdered olx
    ys = getOrdered oly
    ident x y = rebuild $ combine x y

prop_serializePathKey :: PathKey -> Bool
prop_serializePathKey a = case decode (encode a) of
  Left _ -> False
  Right b -> a == b

prop_orderedSerialization :: (Serialize a, Ord a) => [a] -> Bool
prop_orderedSerialization = isSorted . endecode
  where
    endecode :: (Serialize b) => [b] -> [b]
    endecode xs = rights (map decode (sort (map encode xs)))

prop_serializedNumbersOrderable :: [Word] -> Bool
prop_serializedNumbersOrderable = prop_orderedSerialization

prop_serializedPathKeysOrderable :: [PathKey] -> Bool
prop_serializedPathKeysOrderable = prop_orderedSerialization

prop_combineAndRebuildFlattenedDirTrees :: DirTree Int -> DirTree Int -> Bool
prop_combineAndRebuildFlattenedDirTrees base new =
    (ident xs ys) == (xs, ys)
  where
    xs = sort $ flatten base
    ys = sort $ flatten new
    ident a b = rebuild $ combine a b

-- requires prop_serializedPathKeysOrderable
prop_SerializedDirTreeCombineDirTree :: DirTree Int -> DirTree Int -> Bool
prop_SerializedDirTreeCombineDirTree base new =
    (ident xs ys) == (xs, ys)
  where
    xs = sort $ flatten base
    ys = rights . (map decode) . sort . (map encode) $ flatten new
    ident a b = rebuild $ combine a b
