{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Dupes.Index.SQLite (integrationTests) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.String.Interpolate
import qualified Data.Text                        as T
import           Database.SQLite.Simple           as SQLite
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Dupes.FileHash                   as FileHash hiding (integrationTests)
import           Dupes.FileStat                   as FileStat hiding (integrationTests)
import           Pipes
import           Pipes.Safe
import qualified Pipes.SQLite.Simple              as PSQLite
import           System.IO.Temp
import           Test
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

createTableQuery :: Query
createTableQuery =
  queryString
    [i|
    CREATE TABLE #{tableName} (
        path TEXT PRIMARY KEY ASC,
        stat BLOB,
        hash BLOB)
            WITHOUT ROWID;
    CREATE INDEX hash_idx ON Index (hash);|]

queryString :: String -> Query
queryString = Query . T.pack

listDupes :: Connection -> Producer (FileHash, FilePath) (SafeT IO) ()
listDupes connection = PSQLite.query connection query ()
  where
    query :: Query
    query =
      queryString
        [i|
        SELECT hash, self.path
        FROM #{tableName}
        INNER JOIN #{tableName} as self ON hash = self.hash
        GROUP BY hash
        HAVING count(hash) > 1|]

containsPathWithStat :: Connection -> WorkingDirectoryPath -> FileStat -> IO Bool
containsPathWithStat connection path stat = do
  maybeEntry <- getEntryByPath connection path
  return $
    case maybeEntry of
      Just (FileCacheEntry _ stat' _) -> stat == stat'
      Nothing                         -> False

updateEntry :: Connection -> FileCacheEntry -> IO ()
updateEntry connection entry@(FileCacheEntry path _ _) = do
  maybeEntry <- getEntryByPath connection path
  case maybeEntry of
    Just _  -> return ()
    Nothing -> insertEntry connection entry

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

insertEntry :: Connection -> FileCacheEntry -> IO ()
insertEntry connection = execute connection query
  where
    query :: Query
    query = Query $ T.pack [i|INSERT INTO #{tableName} (path, stat, hash) VALUES (?, ?, ?)|]

case_insert_then_get = SQLite.withConnection ":memory:" $ \connection -> do
  SQLite.execute_ connection createTableQuery
  insertEntry connection entry
  maybeEntry' <- getEntryByPath connection path
  Just entry @=? maybeEntry'
  where
    path = WorkingDirectoryPath "file"
    entry = FileCacheEntry path FileStat.create FileHash.nullHash

integrationTests = $(testGroupGenerator)
