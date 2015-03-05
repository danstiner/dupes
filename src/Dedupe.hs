{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Dedupe (
    Condition
  , ConditionM (..)
  , dupes
  , DupesT
  , File (..)
  , htf_thisModulesTests
  , Predicate
  , removeDupes
  , removeDupes'
) where

import           PathSpec
import           Prompt

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Trans.Free
import           Pipes
import qualified Pipes.Prelude            as P

import           Test.Framework

newtype File = File { getFilePath :: FilePath } deriving (Show)

data DupesF m next
  = List (Producer File m () -> next)
  deriving (Functor)

type DupeSet m = Producer File m ()

type DupesT m = FreeT (DupesF m) m

type Predicate = PredicateM Identity
type PredicateM m = File -> m Bool

type Condition = ConditionM Identity

data ConditionM m
  = All [ConditionM m]
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
eval (All conditions)  file = allM (`eval` file) conditions
eval (Any conditions)  file = anyM (`eval` file) conditions
eval (Matches spec)    file = return $ PathSpec.matches spec (getFilePath file)
eval (Not condition)   file = liftM not (eval condition file)
eval (Holds predicate) file = predicate file

-- eval :: Monad m => ConditionM m -> Pipe File File m
-- eval (All conditions)  file = allM (flip eval file) conditions
-- eval (Any conditions)  file = anyM (flip eval file) conditions
-- eval (Matches spec)    file = return $ PathSpec.matches spec (getFilePath file)
-- eval (Not condition)   file = liftM not (eval condition file)
-- eval (Holds predicate) file = predicate file

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f xs = liftM and (mapM f xs)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f xs = liftM or (mapM f xs)

interactive :: PromptM m => ConditionM m
interactive = assert False undefined

runDB :: Monad m => DupesT m a -> m a
runDB = assert False undefined

removeDupes :: Monad m => ConditionM m -> IO ()
removeDupes condition = runEffect $ hoist runDB (dupesByCondition condition) >-> printFile

-- forall sets of dupes containing files matching the condition, remove non-matching dupe files
dupesByCondition :: (Monad m, Monad n) => ConditionM n -> Producer File (DupesT m) ()
dupesByCondition condition = listPossiblyMatchingSets condition >-> filterExactSets condition >-> concatM

listPossiblyMatchingSets ::(Monad m, Monad n) => ConditionM n -> Producer (DupeSet m) (DupesT m) ()
listPossiblyMatchingSets = assert False undefined

filterExactSets :: (Monad m, Monad n) => ConditionM n -> Pipe (DupeSet m) (DupeSet m) (DupesT m) ()
filterExactSets condition = assert False undefined
  -- forever $ do
  -- file <- await
  -- matches <- lift $ eval condition file
  -- when (matches) $ yield file

concatM :: Pipe (Producer a m ()) a (DupesT m) ()
concatM =
  assert False undefined

printFile :: MonadIO m => Consumer File m ()
printFile = P.map getFilePath >-> P.stdoutLn

removeDupes' :: ConditionM (DupesT Identity) -> IO ()
removeDupes' = assert False undefined

dupes :: Monad m => File -> DupesT m (Producer File m ())
dupes = assert False undefined
