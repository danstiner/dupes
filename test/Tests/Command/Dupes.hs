{-# LANGUAGE FlexibleInstances #-}

module Tests.Command.Dupes (tests) where

import Dupes

import Data.Machine
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
        , (testProperty "Encoded SimplePathKeys order as normal" prop_serializedSimplePathKeysOrder)
        , (testProperty "Rebuilding combined path keys is identity" prop_combineAndRebuildSimplePathKeys)
        , (testProperty "Rebuild merged streams in identity" prop_rebuildMergedStreamsInt)]

rebuild :: [MergedOperation a] -> ([a], [a])
rebuild = r
  where
    r [] = ([], [])
    r ((LeftOnly x) : xs) = left x (rebuild xs)
    r (RightOnly x : xs) = right x (rebuild xs)
    r (Both x : xs) = left x $ right x (rebuild xs)
    left x (xs, ys) = (x : xs, ys)
    right y (xs, ys) = (xs, y : ys)

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
    endecode xs = rights . map decode . sort . map encode $ xs

prop_serializedNumbersOrderable :: [Word] -> Bool
prop_serializedNumbersOrderable = prop_orderedSerialization

prop_serializedSimplePathKeysOrder :: [PathKey] -> Bool
prop_serializedSimplePathKeysOrder = prop_orderedSerialization

prop_combineAndRebuildSimplePathKeys :: [PathKey] -> [PathKey] -> Bool
prop_combineAndRebuildSimplePathKeys = prop_combineAndRebuild

prop_combineAndRebuild :: (Ord a) => [a] -> [a] -> Bool
prop_combineAndRebuild xs ys =
    (rebuild $ combine xs ys) == (xs, ys)

prop_rebuildMergedStreamsInt :: [Int] -> [Int] -> Bool
prop_rebuildMergedStreamsInt = prop_rebuildMergedStreams

prop_rebuildMergedStreams :: (Ord a) => [a] -> [a] -> Bool
prop_rebuildMergedStreams xs ys =
  (rebuild . run $ mergeOrderedStreams sourceX sourceY) == (xs, ys)
  where
    sourceX = source xs
    sourceY = source ys

