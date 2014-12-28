{-# LANGUAGE RankNTypes #-}

module Machine (
    group
  , groupBy
) where

import           Control.Applicative
import           Data.Machine

group :: Eq a => Process a [a]
group = groupBy (==)

-- | Group the input by contiguously equal elements into sets
--
-- Does not return empty sets but does properly emit the final non-empty set
groupBy :: (a -> a -> Bool) -> Process a [a]
groupBy eq = repeatedly $ go [] where
  go [] = await >>= go . (:[])
  go acc@(x:_) = do
    i <- await <|> yield (reverse acc) *> stop
    if x `eq` i
      then go (i:acc)
      else yield (reverse acc) *> go [i]
