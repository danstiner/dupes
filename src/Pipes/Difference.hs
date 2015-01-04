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
diff compare = go
  where
    consumeLeft p = lift (next p) >>= either return (\(a, p') -> yield (LeftOnly a) >> consumeLeft p')
    consumeRight p = lift (next p) >>= either return (\(a, p') -> yield (RightOnly a) >> consumeRight p')
    go l r = lift (next l) >>= either (\_ -> consumeRight r) (goRight r)
    goLeft l (rval,r) = lift (next l) >>= either (\_ -> yield (RightOnly rval) >> consumeRight r) (\(lval,l') -> emit compare lval rval l' r)
    goRight r (lval,l) = lift (next r) >>= either (\_ -> yield (LeftOnly lval) >> consumeLeft l) (\(rval,r') -> emit compare lval rval l r')
    emit compare lval rval l r = case compare lval rval of
        LT -> yield (LeftOnly lval) >> goLeft l (rval,r)
        GT -> yield (RightOnly rval) >> goRight r (lval,l)
        EQ -> yield (Common lval rval) >> go l r
{-# INLINABLE diff #-}
