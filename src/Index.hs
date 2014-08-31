{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Index (
    Index
  , RelativeFilePath
  , execIndex
) where

import Control.Monad.Trans


type RelativeFilePath = FilePath

newtype Index m a = Index { runIndex :: m a}

execIndex :: (Monad m) => Index m a -> m a
execIndex = runIndex

instance MonadTrans Index where
  lift = Index
