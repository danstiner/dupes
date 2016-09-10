{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dupes.Index.Internal.SQLite (
    FileCacheEntry(..),
    WorkingDirectoryPath(..),
    updateEntry,
    initialize,
    listDuplicates,
    listHashesWithDuplicates,
    listFilesWithHash,
    testGroup,
    deleteEntryByPath,
    ) where

import           Data.Maybe
import           Data.String.Interpolate
import qualified Data.Text                        as T
import           Database.SQLite.Simple           as SQLite
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Dupes.FileHash                   (FileHash)
import qualified Dupes.FileHash                   as FileHash
import           Dupes.FileStat                   as FileStat
import           Pipes
import qualified Pipes.Prelude                    as P
import           Pipes.Safe
import qualified Pipes.SQLite.Simple              as PSQLite
import           Test.Tasty.HUnit
import           Test.Tasty.TH

newtype WorkingDirectoryPath = WorkingDirectoryPath FilePath
  deriving (Eq, Show)

instance FromField WorkingDirectoryPath where
  fromField field = WorkingDirectoryPath <$> fromField field

instance ToField WorkingDirectoryPath where
  toField (WorkingDirectoryPath path) = toField path

data FileCacheEntry = FileCacheEntry WorkingDirectoryPath FileStat FileHash
  deriving (Eq, Show)

instance FromRow FileCacheEntry where
  fromRow = FileCacheEntry <$> field <*> field <*> field

instance ToRow FileCacheEntry where
  toRow (FileCacheEntry path stat hash) = [toField path, toField stat, toField hash]

tableName :: String
tableName = "FileCache"

initialize :: Connection -> IO ()
initialize connection = SQLite.execute_ connection createTableQuery
  where
    createTableQuery :: Query
    createTableQuery =
      queryString
        [i|
        CREATE TABLE IF NOT EXISTS #{tableName} (
            path TEXT PRIMARY KEY ASC,
            stat BLOB,
            hash BLOB)
                WITHOUT ROWID;
        CREATE INDEX hash_idx ON Index (hash);|]

queryString :: String -> Query
queryString = Query . T.pack

listDuplicates :: Connection -> Producer (FilePath, FileHash) (SafeT IO) ()
listDuplicates connection = PSQLite.query connection query ()
  where
    query :: Query
    query =
      queryString
        [i|
        SELECT entry.path, grouping.hash
        FROM (SELECT hash
              FROM #{tableName}
              GROUP BY hash
              HAVING count() > 1) AS grouping
        JOIN #{tableName} as entry ON entry.hash = grouping.hash
        ORDER BY grouping.hash|]

listHashesWithDuplicates :: Connection -> Producer FileHash (SafeT IO) ()
listHashesWithDuplicates connection = PSQLite.query connection query () >-> P.map unOnly
  where
    query :: Query
    query =
      queryString
        [i|
        SELECT hash
        FROM #{tableName}
        GROUP BY hash
        HAVING count() > 1|]

listFilesWithHash :: Connection -> FileHash -> Producer FilePath (SafeT IO) ()
listFilesWithHash connection hash = PSQLite.query connection query (Only hash) >-> P.map unOnly
  where
    query :: Query
    query =
      queryString
        [i|
        SELECT path
        FROM #{tableName}
        WHERE hash = ?|]

unOnly :: Only x -> x
unOnly (Only x) = x

containsPathWithStat :: Connection -> WorkingDirectoryPath -> FileStat -> IO Bool
containsPathWithStat connection path stat = do
  maybeEntry <- getEntryByPath connection path
  return $
    case maybeEntry of
      Just (FileCacheEntry _ stat' _) -> stat == stat'
      Nothing                         -> False

updateEntry :: Connection -> FileCacheEntry -> IO ()
updateEntry connection entry@(FileCacheEntry path stat hash) = do
  maybeEntry <- getEntryByPath connection path
  case maybeEntry of
    Just _  -> SQLite.execute connection updateQuery (stat, hash, path)
    Nothing -> addEntry connection entry

  where
    updateQuery = queryString
                    [i|
        UPDATE #{tableName}
        SET stat = ?, hash = ?
        WHERE path = ?|]

deleteEntryByPath :: Connection -> WorkingDirectoryPath -> IO ()
deleteEntryByPath connection path =
  SQLite.execute connection query (Only path)
  where
    query =
      queryString
        [i|
          DELETE
          FROM #{tableName}
          WHERE path = ?|]

getEntryByPath :: Connection -> WorkingDirectoryPath -> IO (Maybe FileCacheEntry)
getEntryByPath connection path =
  listToMaybe <$> SQLite.query connection query (Only path)
  where
    query =
      queryString
        [i|
        SELECT path, stat, hash
        FROM #{tableName}
        WHERE path = ?|]

addEntry :: Connection -> FileCacheEntry -> IO ()
addEntry connection = execute connection query
  where
    query :: Query
    query = Query $ T.pack [i|INSERT INTO #{tableName} (path, stat, hash) VALUES (?, ?, ?)|]

case_add_then_get = SQLite.withConnection ":memory:" $ \connection -> do
  initialize connection
  addEntry connection entry
  maybeEntry' <- getEntryByPath connection path
  Just entry @=? maybeEntry'
  where
    path = WorkingDirectoryPath "file"
    entry = FileCacheEntry path FileStat.create FileHash.nullHash

case_add_then_contains = SQLite.withConnection ":memory:" $ \connection -> do
  initialize connection
  addEntry connection entry
  contained <- containsPathWithStat connection path stat
  assertBool "Contains should be true after add" contained
  where
    path = WorkingDirectoryPath "file"
    stat = FileStat.create
    entry = FileCacheEntry path stat FileHash.nullHash

case_add_dupes_then_list = SQLite.withConnection ":memory:" $ \connection -> do
  initialize connection
  addEntry connection entry1
  addEntry connection entry2
  dupes <- runSafeT $ P.toListM (listDuplicates connection)
  2 @=? length dupes
  where
    path1 = WorkingDirectoryPath "file1"
    path2 = WorkingDirectoryPath "file2"
    stat = FileStat.create
    entry1 = FileCacheEntry path1 stat FileHash.nullHash
    entry2 = FileCacheEntry path2 stat FileHash.nullHash

case_update_with_no_previous_entry_adds = SQLite.withConnection ":memory:" $ \connection -> do
  initialize connection
  updateEntry connection entry
  maybeEntry' <- getEntryByPath connection path
  Just entry @=? maybeEntry'
  where
    path = WorkingDirectoryPath "file"
    entry = FileCacheEntry path FileStat.create FileHash.nullHash

case_update_with_previous_entry_updates = SQLite.withConnection ":memory:" $ \connection -> do
  initialize connection
  addEntry connection entryPrevious
  updateEntry connection entryNew
  maybeEntry' <- getEntryByPath connection path
  Just entryNew @=? maybeEntry'
  where
    path = WorkingDirectoryPath "file"
    entryPrevious = FileCacheEntry path FileStat.create (FileHash.hashByteString "0")
    entryNew = FileCacheEntry path FileStat.create (FileHash.hashByteString "1")

testGroup = $(testGroupGenerator)
