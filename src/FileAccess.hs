{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module FileAccess (module FileAccess) where

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           System.Directory      as Directory
import           System.FilePath       as FilePath

data FileAccessF next = DoesDirectoryExist FilePath (Bool -> next)
                      | ParentDirectory FilePath (FilePath -> next)
                      | CreateDirectoryIfMissing FilePath (next)
  deriving Functor

type FileAccess = Free FileAccessF

makeFree ''FileAccessF

runIO :: FileAccess a -> IO a
runIO (Free (DoesDirectoryExist path f)) = Directory.doesDirectoryExist path >>= runIO . f
runIO (Free (ParentDirectory path f)) = runIO . f $ FilePath.takeDirectory path
runIO (Free (CreateDirectoryIfMissing path f)) = Directory.createDirectoryIfMissing False path >> runIO
                                                                                                    f
runIO (Pure a) = return a

runPure :: [FilePath] -> FileAccess a -> a
runPure paths (Free (DoesDirectoryExist path f)) = runPure paths . f $ path `elem` paths
runPure paths (Free (ParentDirectory path f)) = runPure paths . f $ takeDirectory path
runPure paths (Free (CreateDirectoryIfMissing path f)) = runPure (path : paths) f
runPure _ (Pure a) = a

parentDirectories :: FilePath -> FileAccess [FilePath]
parentDirectories path = do
  parent <- FileAccess.parentDirectory path
  if path == parent
    then return [path]
    else fmap (path :) (parentDirectories parent)
