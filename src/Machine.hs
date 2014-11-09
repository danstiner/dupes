{-# LANGUAGE RankNTypes #-}

module Machine (
  groupBy
) where

import Control.Applicative
import Data.Machine

-- | Group the input by contiguously equal elements into sets
--
-- Does not return empty sets but does properly emit the final non-empty set
groupBy :: (a -> a -> Bool) -> Process a [a]
groupBy eq = repeatedly $ go [] where
  go [] = await >>= go . (:[])
  go acc@(x:_) = do
    i <- await <|> yield (reverse acc) *> stop
    if (eq x i)
      then go (i:acc)
      else yield (reverse acc) *> go [i]
