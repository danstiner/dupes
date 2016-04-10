{-# LANGUAGE TemplateHaskell #-}

module Database.SQLite (open, integrationTests) where

import           Control.Applicative
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as SQLite

import           Test.Tasty.HUnit
import           Test.Tasty.TH

import           Repository             (Repository)

data DB = DB

data TestField = TestField Int String
  deriving Show

instance SQLite.FromRow TestField where
  fromRow = TestField <$> SQLite.field <*> SQLite.field

open :: Repository -> IO DB
open r = undefined

case_connection_open_and_close = do
  connection <- SQLite.open ":memory:"
  SQLite.execute_ connection
    (SQLite.Query $ T.pack "CREATE TABLE test (id INTEGER PRIMARY KEY, str text)")
  SQLite.execute connection (SQLite.Query $ T.pack "INSERT INTO test (str) VALUES (?)")
    (SQLite.Only "value")
  [TestField id value] <- SQLite.query_ connection (SQLite.Query $ T.pack "SELECT * FROM test") :: IO [TestField]
  "value" @=? value
  SQLite.close connection

integrationTests = $(testGroupGenerator)