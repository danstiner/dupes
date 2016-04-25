{-# LANGUAGE TemplateHaskell #-}

module Dupes.Index (IndexPath, withIndex, updateFile, construct) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Database.SQLite             as SQLite
import           Dupes.FileHash              as FileHash
import           Dupes.FileStat              as FileStat
import           Dupes.Index.Internal.SQLite as Internal
import           Logging
import           System.FilePath

data Index = Index SQLite.Connection

newtype IndexPath = IndexPath FilePath
  deriving Show

construct :: FilePath -> IndexPath
construct = IndexPath . (</> "index.sqlite")

withIndex :: (MonadMask m, MonadIO m) => IndexPath -> (Index -> m a) -> m a
withIndex (IndexPath path) f = SQLite.with path
                                 (\connection ->
                                    liftIO (Internal.initialize connection) >> (f . Index)
                                                                                 connection)

updateFile :: Index -> FilePath -> IO ()
updateFile (Index connection) path = do
  entry <- computeEntry path
  Internal.updateEntry connection entry

  where
    computeEntry :: FilePath -> IO FileCacheEntry
    computeEntry path = do
      maybeHash <- FileHash.hashFile path
      case maybeHash of
        Left errorMsg -> exitErrorM $(logTag) (path ++ ": " ++ errorMsg) >> return undefined
        Right hash    -> return (FileCacheEntry (WorkingDirectoryPath path) FileStat.create hash)
