{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Index (
    Action (..)
  , FileMode
  , Entry
  , Index
  , RelativeFilePath
  , set
  , execIndex
) where

import qualified Blob

newtype FileMode = FileMode Int
type RelativeFilePath = FilePath
data Entry = Entry RelativeFilePath FileMode Blob.Id

data Action = Set RelativeFilePath Blob.Id | Read

type Actions = [Action]

newtype Index a = Index { runIndex :: (a, Actions)}

instance Monad Index where
  return a = Index (a, [])
  m >>= k = let (a, w) = runIndex m
                n      = k a
                (b, x) = runIndex n
            in Index (b, w ++ x)

set :: RelativeFilePath -> Blob.Id -> Index ()
set path hash = Index ((), [Set path hash])

execIndex :: (Monad m) => Index a -> (Action -> m ()) -> m a
execIndex m f = do
  mapM f actions
  return result
  where
    ran = runIndex m
    result = fst ran
    actions = snd ran
