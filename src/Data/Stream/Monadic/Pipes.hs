{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Stream.Monadic.Pipes
    (
      fromStream
    ) where

import Pipes
import Database.LevelDB.Streaming

fromStream :: (Monad m) => Stream m Entry -> Producer Entry m ()
fromStream (Stream next s0) = go =<< lift s0
  where
    go !s = do
        step <- lift (next s)
        case step of
            Done -> return ()
            Skip s' -> go s'
            Yield x s' -> yield x >> go s'
{-# INLINE fromStream #-}
