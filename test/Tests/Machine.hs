module Tests.Machine (tests) where

import Machine

import Data.Machine

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Property

tests :: [Test]
tests = [ (testProperty "concatenation of result equal to input" prop_concat)
        , (testProperty "sublists contain only equal elements" prop_equalelem)
        , (testProperty "adjacent sublists do not contain equal elements" prop_adjacentsNotEqual)]

prop_concat :: [Int] -> Test.QuickCheck.Property.Result
prop_concat xs =
  MkResult (Just $ concat grouped == xs) True msg False False [] []
  where
    grouped = run (source xs ~> groupBy (==))
    msg = "grouping: " ++ (show grouped)

prop_equalelem :: [Int] -> Test.QuickCheck.Property.Result
prop_equalelem xs =
  MkResult (Just $ all sublistEq grouped) True msg False False [] []
  where
    grouped = run (source xs ~> groupBy (==))
    msg = "grouping: " ++ (show grouped)
    sublistEq [] = True
    sublistEq (x:xs) = all (==x) xs

prop_adjacentsNotEqual :: [Int] -> Test.QuickCheck.Property.Result
prop_adjacentsNotEqual xs =
  MkResult (Just $ adjNotEqual grouped) True msg False False [] []
  where
    grouped = run (source xs ~> groupBy (==))
    msg = "grouping: " ++ (show grouped)
    adjNotEqual (a:b:xs) = (not $ eq a b) && adjNotEqual (b:xs)
    adjNotEqual _ = True
    eq (a:_) (b:_) = a == b
    eq _ _ = False
