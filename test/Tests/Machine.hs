module Tests.Machine (tests) where

import Machine

import Data.Machine

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Property as Property

tests :: [Test]
tests = [ (testProperty "concatenation of result equal to input" prop_groupBy_concat)
        , (testProperty "sublists contain only equal elements" prop_groupBy_equalelem)
        , (testProperty "adjacent sublists do not contain equal elements" prop_groupBy_adjacentsNotEqual)]

prop_groupBy_concat :: [Int] -> Property.Result
prop_groupBy_concat xs =
  mkResult (Just $ concat grouped == xs) msg
  where
    grouped = run (source xs ~> groupBy (==))
    msg = "grouping: " ++ (show grouped)

prop_groupBy_equalelem :: [Int] -> Property.Result
prop_groupBy_equalelem xs =
  mkResult (Just $ all sublistEq grouped) msg
  where
    grouped = run (source xs ~> groupBy (==))
    msg = "grouping: " ++ (show grouped)
    sublistEq [] = True
    sublistEq (x:xs) = all (==x) xs

prop_groupBy_adjacentsNotEqual :: [Int] -> Property.Result
prop_groupBy_adjacentsNotEqual xs =
  mkResult (Just $ adjNotEqual grouped) msg
  where
    grouped = run (source xs ~> groupBy (==))
    msg = "grouping: " ++ (show grouped)
    adjNotEqual (a:b:xs) = (not $ eq a b) && adjNotEqual (b:xs)
    adjNotEqual _ = True
    eq (a:_) (b:_) = a == b
    eq _ _ = False

mkResult :: Maybe Bool -> String -> Property.Result
mkResult result msg = MkResult result True msg Nothing False [] []
