module Pipes.Difference
    (
      SequenceDifference (..)
    , diff
    ) where

import           Pipes

data SequenceDifference a b
  = LeftOnly a
  | RightOnly b
  | Common a b

diff :: (Monad m)
      => (a -> b -> Ordering) -- ^ ordering on elements
      -> Producer a m ()      -- ^ source producer of elements
      -> Producer b m ()      -- ^ compare to producer of elements
      -> Producer (SequenceDifference a b) m ()
diff compare left right = go left right
  where
    -- consumeLeft :: (Monad m) => Producer a m () -> Producer (SequenceDifference a b) m ()
    consumeLeft p = lift (next p) >>= either return (\(a, p') -> yield (LeftOnly a) >> consumeLeft p')
    -- consumeRight :: (Monad m) => Producer b m () -> Producer (SequenceDifference a b) m ()
    consumeRight p = lift (next p) >>= either return (\(a, p') -> yield (RightOnly a) >> consumeRight p')
    -- go :: (Monad m) => Producer a m () -> Producer b m () -> Producer (SequenceDifference a b) m ()
    go l r = lift (next l) >>= either (\_ -> consumeRight r) (goRight r)
    -- goLeft :: (Monad m) => Producer a m () -> (b, Producer b m ()) -> Producer (SequenceDifference a b) m ()
    goLeft l (rval,r) = (lift $ next l) >>= either (\_ -> yield (RightOnly rval) >> consumeRight r) (\(lval,l') -> emit compare lval rval l' r)
    -- goRight :: (Monad m) => Producer b m () -> (a, Producer a m ()) -> Producer (SequenceDifference a b) m ()
    goRight r (lval,l) = (lift $ next r) >>= either (\_ -> yield (LeftOnly lval) >> consumeLeft l) (\(rval,r') -> emit compare lval rval l r')
    -- emit :: (Monad m) => (a -> b -> Ordering) -> a -> b -> Producer a m () -> Producer b m () -> Producer (SequenceDifference a b) m ()
    emit compare lval rval l r = case compare lval rval of
        LT -> yield (LeftOnly lval) >> goLeft l (rval,r)
        GT -> yield (RightOnly rval) >> goRight r (lval,l)
        EQ -> yield (Common lval rval) >> go l r
{-# INLINABLE diff #-}
