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
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test, path)
import Test.QuickCheck

tests :: [Test]
tests = [ testStringEquality
        , prop_stringEquals
        , prop_rebuiltArray
        , (testProperty "path key serialization" prop_serializePathKey)
        , (testProperty "path key ordering" prop_orderedPathKeys)
        , (testProperty "dirtree merging" prop_dirTreeMergeUnMerge)
        , (testProperty "ordered numbers" prop_orderedNumbers)
        , (testProperty "files merging" prop_filesMergeUnMerge)
        , (testProperty "dirtree serialized ordering" prop_dirTreeSerializedSortCombine)]

testStringEquality :: Test
testStringEquality = testCase "string equality" $ do
  assertEqual "string equality" "string1" "string1"

prop_stringEquals :: Test
prop_stringEquals = testProperty "prop_stringEquals " $ \s ->
  s == (s :: String)

prop_rebuiltArray :: Test
prop_rebuiltArray = testProperty "prop_rebuiltArray" $ \(xs, ys) ->
  let sxs = sort xs :: [Int] in
  let sys = sort ys :: [Int] in
  (ident sxs sys) == (sxs, sys)
  where
    ident xs ys = rebuild $ combine xs ys

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

prop_serializePathKey :: PathKey -> Bool
prop_serializePathKey a = case decode (encode a) of
  Left _ -> False
  Right b -> a == b

prop_orderedSerialization :: (Serialize a, Ord a) => [a] -> Bool
prop_orderedSerialization = isSorted . endecode
  where
    endecode :: (Serialize b) => [b] -> [b]
    endecode xs = rights (map decode (sort (map encode xs)))

prop_orderedNumbers :: [Word] -> Bool
prop_orderedNumbers = prop_orderedSerialization

prop_orderedPathKeys :: [PathKey] -> Bool
prop_orderedPathKeys = prop_orderedSerialization

prop_filesMergeUnMerge :: OrderedList (PathKey, Int) -> OrderedList (PathKey, Int) -> Bool
prop_filesMergeUnMerge xo yo =
    (ident xs ys) == (xs, ys)
  where
    xs = getOrdered xo
    ys = getOrdered yo
    ident a b = rebuild $ combine a b

prop_dirTreeMergeUnMerge :: DirTree Int -> DirTree Int -> Bool
prop_dirTreeMergeUnMerge base new =
    (ident xs ys) == (xs, ys)
  where
    xs = sort $ flatten base
    ys = sort $ flatten new
    ident a b = rebuild $ combine a b

prop_dirTreeSerializedSortCombine :: DirTree Int -> DirTree Int -> Bool
prop_dirTreeSerializedSortCombine base new =
    (ident xs ys) == (xs, ys)
  where
    xs = sort $ flatten base
    ys = rights . (map decode) . sort . (map encode) $ flatten new
    ident a b = rebuild $ combine a b
