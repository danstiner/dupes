{-# LANGUAGE RankNTypes #-}

module Pipes.Parse.Ext (parsedForever_) where

import           Pipes
import           Pipes.Parse

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
