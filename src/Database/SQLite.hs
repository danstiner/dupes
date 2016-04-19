{-# LANGUAGE TemplateHaskell #-}

module Database.SQLite (open, integrationTests) where

import           Control.Applicative
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as SQLite
import           Dupes.Repository       (Repository)
import qualified Dupes.Repository       as Repository
import           System.IO.Temp
import           Test
import           Test.Tasty.HUnit
import           Test.Tasty.TH

data DBConnection = DBConnection

open :: Repository -> IO DBConnection
open r = return DBConnection

close :: DBConnection -> IO ()
close c = return ()

case_connection_open_and_close = withSystemTempDirectory $(tempNameTemplate) $ \path -> do
  connection <- open =<< Repository.initialize path
  close connection

data TestField = TestField Int String
  deriving Show

instance SQLite.FromRow TestField where
  fromRow = TestField <$> SQLite.field <*> SQLite.field

case_sql_insert_a_value = do
  connection <- SQLite.open ":memory:"
  SQLite.execute_ connection
    (SQLite.Query $ T.pack "CREATE TABLE test (id INTEGER PRIMARY KEY, str text)")
  SQLite.execute connection (SQLite.Query $ T.pack "INSERT INTO test (str) VALUES (?)")
    (SQLite.Only "value")
  [TestField id value] <- SQLite.query_ connection (SQLite.Query $ T.pack "SELECT * FROM test") :: IO [TestField]
  "value" @=? value
  SQLite.close connection

integrationTests = $(testGroupGenerator)
