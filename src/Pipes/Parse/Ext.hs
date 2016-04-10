{-# LANGUAGE RankNTypes #-}

module Pipes.Parse.Ext (parsedForever_) where

import           Pipes
import           Pipes.Parse

{-| Run a `Parser` repeatedly on a `Producer`, `yield`ing each `Right` result

    Returns the remainder of the `Producer` when the `Parser` returns `Left`
-}
parsed :: Monad m
       => Parser a m (Either e b)
       -> Producer a m r -> Producer b m (e, Producer a m r)
parsed parser = go
  where
    go p = do
      (x, p') <- lift (runStateT parser p)
      case x of
        Left r -> return (r, p')
        Right b -> do
          yield b
          go p'

{-| Run a `Parser` repeatedly on a `Producer`, `yield`ing each `Just` result

    Sequences the remainder of the `Producer` when the `Parser` returns `Just`
-}
parsedForever_ :: Monad m
               => Parser a m (Maybe a)
               -> Producer a m ()
               -> Producer a m ()
parsedForever_ parser p = do
  ((), p') <- parsed parser' p
  p'

  where
    parser' = do
      x <- parser
      return
        (case x of
           Nothing -> Left ()
           Just b  -> Right b)
