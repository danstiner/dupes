{-# LANGUAGE TemplateHaskell #-}

module Database.SQLite (with, Connection, integrationTests) where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.Text              as T
import           Database.SQLite.Simple as SQLite
import           System.IO.Temp
import           Test
import           Test.Tasty.HUnit
import           Test.Tasty.TH

with :: MonadIO m => FilePath -> (Connection -> m a) -> m a
with = undefined

case_connection_open_and_close = withSystemTempFile $(tempNameTemplate) $ \path _ -> do
  connection <- open path
  close connection

data TestField = TestField Int String
  deriving Show

instance SQLite.FromRow TestField where
  fromRow = TestField <$> SQLite.field <*> SQLite.field

case_sql_insert_a_value = SQLite.withConnection ":memory:" $ \connection -> do
  SQLite.execute_ connection (SQLite.Query $ T.pack "CREATE TABLE test (id INTEGER PRIMARY KEY, str text)")
  SQLite.execute connection (SQLite.Query $ T.pack "INSERT INTO test (str) VALUES (?)") (SQLite.Only "value")
  [TestField id value] <- SQLite.query_ connection (SQLite.Query $ T.pack "SELECT * FROM test") :: IO [TestField]
  "value" @=? value

integrationTests = $(testGroupGenerator)
