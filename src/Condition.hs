{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Condition (Condition, ConditionM(..), Predicate) where

import           File
import           PathSpec

import           Control.Monad.Identity

import           Test.Framework

type Predicate = PredicateM Identity

type PredicateM m = File -> m Bool

type Condition = ConditionM Identity

data ConditionM m = All [ConditionM m]
                  | Any [ConditionM m]
                  | Holds (PredicateM m)
                  | Matches PathSpec
                  | Not (ConditionM m)

test_condition_all_false = assertBool . not $ evalPure condition (File path)
  where
    condition = All [false, true]
    false = Not true
    true = Matches (PathSpec.parse path)
    path = "file"

test_condition_all_true = assertBool $ evalPure condition (File path)
  where
    condition = All [true, true]
    true = Matches (PathSpec.parse path)
    path = "file"

test_condition_any_true = assertBool $ evalPure condition (File path)
  where
    condition = Any [false, true]
    false = Not true
    true = Matches (PathSpec.parse path)
    path = "file"

test_condition_any_false = assertBool . not $ evalPure condition (File path)
  where
    condition = Any [false, false]
    false = Not true
    true = Matches (PathSpec.parse path)
    path = "file"

test_condition_matchingPathSpec = assertBool $ evalPure condition (File path)
  where
    condition = Matches (PathSpec.parse path)
    path = "file"

test_condition_holds = assertBool $ evalPure condition (File path)
  where
    condition = Holds (const $ return True)
    path = "file"

evalPure :: Condition -> File -> Bool
evalPure condition file = runIdentity $ eval condition file

eval :: Monad m => ConditionM m -> File -> m Bool
eval (All conditions) file = allM (`eval` file) conditions
eval (Any conditions) file = anyM (`eval` file) conditions
eval (Matches spec) file = return $ PathSpec.matches spec (getFilePath file)
eval (Not condition) file = liftM not (eval condition file)
eval (Holds predicate) file = predicate file

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f xs = liftM and (mapM f xs)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f xs = liftM or (mapM f xs)
