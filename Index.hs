{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Index (
    Index
  , IndexT
  , FileMode
  , MonadIndex (..)
  , exec
) where

import Data.Functor.Identity

import qualified Blob

newtype FileMode = FileMode Int
type RelativeFilePath = FilePath
data Entry = Entry RelativeFilePath FileMode Blob.Id

class Monad m => MonadIndex m where
  read :: RelativeFilePath -> m Entry
  set :: RelativeFilePath -> FileMode -> Blob.Id -> m ()

instance Monad m => MonadIndex (IndexT s m) where
    read = iread
    set = iset

iread :: (Monad m) => RelativeFilePath -> IndexT s m Entry
iread = do fail "TODO"

iset :: (Monad m) => RelativeFilePath -> FileMode -> Blob.Id -> IndexT s m ()
iset = do fail "Blerg"

type Index s = IndexT s Identity

newtype IndexT s m a = IndexT { runIndexT :: s -> m (a, s) }


instance (Monad m) => Monad (IndexT s m) where
    return a = state $ \s -> (a, s)
    m >>= k  = IndexT $ \s -> do
        ~(a, s') <- runIndexT m s
        runIndexT (k a) s'
    fail str = IndexT $ \_ -> fail str

state :: Monad m
      => (s -> (a, s))  -- ^pure state transformer
      -> IndexT s m a   -- ^equivalent state-passing computation
state f = IndexT (return . f)

exec :: Index s a -> s -> (a, s)
exec m = runIdentity . runIndexT m
