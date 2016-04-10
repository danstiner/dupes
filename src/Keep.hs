{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Keep ( pureTests ) where

import Control.Applicative
import Control.Monad
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as P
import qualified Control.DeepSeq as DeepSeq

import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

data Which = LeftOnly | RightOnly | Both deriving (Show)

instance Arbitrary Which where
  arbitrary = elements [LeftOnly, RightOnly, Both]

parser :: (Monad m) => (a -> a -> Which) -> Parser a m (Maybe a)
parser which = do
  drawA <- draw
  case drawA of
    Nothing -> return Nothing
    Just a -> do
      drawB <- draw
      case drawB of
        Nothing -> return (Just a)
        Just b -> case which a b of
            LeftOnly  -> return (Just a)
            RightOnly -> return (Just b)
            Both      -> unDraw b *> return (Just a)

{-| Run a `Parser` repeatedly on a `Producer`, `yield`ing each `Right` result

    Returns the remainder of the `Producer` when the `Parser` returns `Left`
-}
parsed
    :: Monad m
    => Parser a m (Either e b)
    -> Producer a m r -> Producer b m (e, Producer a m r)
parsed parser = go
  where
    go p = do
        (x, p') <- lift (runStateT parser p)
        case x of
            Left  r -> return (r, p')
            Right b -> do
                yield b
                go p'

{-| Run a `Parser` repeatedly on a `Producer`, `yield`ing each `Just` result

    Sequences the remainder of the `Producer` when the `Parser` returns `Just`
-}
parsedForever_
    :: Monad m
    => Parser a m (Maybe a)
    -> Producer a m ()
    -> Producer a m ()
parsedForever_ parser p = do
    ((), p') <- parsed parser' p
    p'
  where
    parser' = do
        x <- parser
        return (case x of
            Nothing -> Left ()
            Just b  -> Right b )

filterKeeping :: (Monad m) => (a -> a -> Which) -> Producer a m () -> Producer a m ()
filterKeeping which = parsedForever_ (parser which)

filterListKeeping :: (a -> a -> Which) -> [a] -> [a]
filterListKeeping which xs = P.toList (filterKeeping which (each xs)) 

prop_filter_keep_both_is_id :: [Int] -> Bool
prop_filter_keep_both_is_id xs = filterListKeeping (const . const Both) xs == xs

prop_filter_keeps_at_least_one :: NonEmptyList Int -> Property
prop_filter_keeps_at_least_one (NonEmpty xs) = forAll (filterArbitrarily xs) (not . null)
  where
    filterArbitrarily :: [a] -> Gen [a]
    filterArbitrarily xs = do
      arbitraryWhich <- const . const <$> arbitrary
      return $ filterListKeeping arbitraryWhich xs

prop_filter_terminates :: [Int] -> Which -> Bool
prop_filter_terminates xs which = seq (filterListKeeping (const . const which) xs) True

pureTests = $(testGroupGenerator)
