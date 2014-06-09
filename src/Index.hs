{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Index (
    IndexMonad (..)
  , FileMode
  , Entry
  , Index
  , RelativeFilePath
  , execIndex
) where

import Control.Monad.Trans

import qualified Blob

newtype FileMode = FileMode Int
type RelativeFilePath = FilePath
type Key = RelativeFilePath
data Entry = Entry RelativeFilePath FileMode Blob.Id

newtype Index m a = Index { runIndex :: m a}

class (Monad m) => IndexMonad m where
  read :: Key -> Index m Entry
  set :: Key -> Blob.Id -> Index m ()

execIndex :: (Monad m) => Index m a -> m a
execIndex = runIndex

instance MonadTrans Index where
  lift = Index
