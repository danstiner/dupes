{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Stream.Monadic.Pipes (fromStream) where

import           Database.LevelDB.Streaming
import           Pipes

fromStream :: Monad m => Stream m a -> Producer a m ()
fromStream (Stream nextStep s0) = go =<< lift s0
  where
    go !s = do
      step <- lift (nextStep s)
      case step of
        Done       -> return ()
        Skip s'    -> go s'
        Yield x s' -> yield x >> go s'

{-# INLINE fromStream #-}
