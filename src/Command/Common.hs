module Command.Common (
    adapt
  , adapt'
  , printPaths
  , filterDuplicates
  , filterDuplicates'
  ) where

import           DuplicateCache
import           Repository                   as R

import           Control.Exception
import           Control.Monad.Free
import           Control.Monad.Morph
import           Control.Monad.Trans.Resource
import           Pipes
import qualified Pipes.Prelude                as P


adapt :: (MMonad t, MonadResource m) => RepositoryHandle -> t (DuplicateCacheT m) b -> t m b
adapt handle = assert False undefined -- embed (lift . runFOnDB (getCache handle))

adapt' :: MonadResource m => RepositoryHandle -> (DuplicateCacheT m) a -> m a
adapt' handle = runFOnDB (getCache handle)

printPaths :: MonadIO m => Consumer HashPath m ()
printPaths = P.map getFilePath >-> P.stdoutLn

filterDuplicates :: Monad m => (HashPath -> Producer HashPath m () -> m Bool) -> Pipe HashPath HashPath (DuplicateCacheT m) ()
filterDuplicates f = P.filterM (filterDuplicate f)

filterDuplicates' :: DuplicateCacheC m => (HashPath -> Producer HashPath m () -> m Bool) -> Pipe HashPath HashPath m ()
filterDuplicates' f = P.filterM (filterDuplicate' f)

filterDuplicate :: Monad m => (HashPath -> Producer HashPath m () -> m Bool) -> HashPath -> DuplicateCacheT m Bool
filterDuplicate f hashpath = do
  d <- dupes
  r <- lift $ f hashpath d
  lift . return $ r
  -- Pure r -- >-> excludeSelf
  where
    dupes = listDupesF $ getFileHash hashpath
    -- excludeSelf = P.filter (\a -> getFilePath a /= (getFilePath hashpath))


filterDuplicate' :: DuplicateCacheC m => (HashPath -> Producer HashPath m () -> m Bool) -> HashPath -> m Bool
filterDuplicate' f hashpath = do
	f hashpath dupes

	--f hashpath $ dupes >-> excludeSelf
  where
    dupes = listDupesC $ getFileHash hashpath
    -- excludeSelf = P.filter (\a -> getFilePath a /= (getFilePath hashpath))

convert :: Monad m => m a -> DuplicateCacheT m a
convert = assert False undefined

