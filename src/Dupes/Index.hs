{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Dupes.Index (
    IndexPath,
    Index,
    withIndex,
    updateFile,
    construct,
    listDuplicates,
    listHashesWithDuplicates,
    listFilesWithHash,
    listAll,
    deleteEntryByPath,
    ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Database.SQLite             as SQLite
import           Dupes.FileHash              as FileHash
import           Dupes.FileStat              as FileStat
import qualified Dupes.Index.Internal.SQLite as Internal
import           Logging
import           Pipes
import           Pipes.Safe
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
    computeEntry :: FilePath -> IO Internal.FileCacheEntry
    computeEntry path = do
      maybeHash <- FileHash.hashFile path
      case maybeHash of
        Left errorMsg -> exitErrorM $(logTag) (path ++ ": " ++ errorMsg) >> return undefined
        Right hash -> return
                        (Internal.FileCacheEntry
                           (Internal.WorkingDirectoryPath path)
                           FileStat.create
                           hash)

listDuplicates :: Index -> Producer (FilePath, FileHash) (SafeT IO) ()
listDuplicates (Index connection) = Internal.listDuplicates connection

listHashesWithDuplicates :: Index -> Producer FileHash (SafeT IO) ()
listHashesWithDuplicates (Index connection) = Internal.listHashesWithDuplicates connection

listFilesWithHash :: Index -> FileHash -> Producer FilePath (SafeT IO) ()
listFilesWithHash (Index connection) = Internal.listFilesWithHash connection

listAll :: Index -> Producer FilePath (SafeT IO) ()
listAll = undefined

deleteEntryByPath :: Index -> FilePath -> IO ()
deleteEntryByPath (Index connection) path = Internal.deleteEntryByPath connection
                                              (Internal.WorkingDirectoryPath path)
