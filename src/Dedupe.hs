{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Dedupe (
    Condition,
    ConditionM(..),
    dupes,
    DupesT,
    File(..),
    htf_thisModulesTests,
    Predicate,
    removeDupes,
    removeDupes',
    ) where

import           Condition
import           File
import           PathSpec
import           Prompt

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Trans.Free
import           Pipes
import qualified Pipes.Prelude            as P

import           Test.Framework

data DupesF m next = List (Producer File m () -> next)
  deriving Functor

type DupeSet m = Producer File m ()

type DupesT m = FreeT (DupesF m) m

interactive :: PromptM m => ConditionM m
interactive = assert False undefined

runDB :: Monad m => DupesT m a -> m a
runDB = assert False undefined

removeDupes :: Monad m => ConditionM m -> IO ()
removeDupes condition = runEffect $ hoist runDB (dupesByCondition condition) >-> printFile

-- forall sets of dupes containing files matching the condition, remove non-matching dupe files
dupesByCondition :: (Monad m, Monad n) => ConditionM n -> Producer File (DupesT m) ()
dupesByCondition condition = listPossiblyMatchingSets condition >->
                             filterExactSets condition >->
                             concatM

listPossiblyMatchingSets :: (Monad m, Monad n) => ConditionM n -> Producer (DupeSet m) (DupesT m) ()
listPossiblyMatchingSets = assert False undefined

filterExactSets :: (Monad m, Monad n) => ConditionM n -> Pipe (DupeSet m) (DupeSet m) (DupesT m) ()
filterExactSets condition = assert False undefined

-- forever $ do file <- await matches <- lift $ eval condition file when (matches) $ yield file
concatM :: Pipe (Producer a m ()) a (DupesT m) ()
concatM =
  assert False undefined

printFile :: MonadIO m => Consumer File m ()
printFile = P.map getFilePath >-> P.stdoutLn

removeDupes' :: ConditionM (DupesT Identity) -> IO ()
removeDupes' = assert False undefined

dupes :: Monad m => File -> DupesT m (Producer File m ())
dupes = assert False undefined
