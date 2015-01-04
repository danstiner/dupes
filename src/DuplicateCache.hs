{-# LANGUAGE RankNTypes #-}

module DuplicateCache (
    DuplicateCache (..)
    , update
    , open
) where

import           Index                      (IndexChange (..))

import           Control.Exception
import           Control.Monad
import           Database.LevelDB           hiding (open)
import           Database.LevelDB.Streaming
import           Pipes

data DuplicateCache = DuplicateCache DB

open :: DB -> DuplicateCache
open = DuplicateCache

update :: (MonadResource m) => DuplicateCache -> Consumer IndexChange m ()
update cache = forever $ do
    a <- await
    liftIO $ p a
    return ()
  where
  	p (Insert path) = putStr "+ " >> putStrLn path
  	p (Remove path) = putStr "- " >> putStrLn path
  	p _ = return ()
