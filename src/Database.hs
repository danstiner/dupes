{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Database (open, htf_thisModulesTests) where

import           Control.Applicative
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as SQLite

import           Test.Framework
import           Test.HUnit

import           Repository             (Repository)

data DB = DB

data TestField = TestField Int String
  deriving Show

instance SQLite.FromRow TestField where
  fromRow = TestField <$> SQLite.field <*> SQLite.field

open :: Repository -> IO DB
open r = undefined

test_openAndClose_Connection = do
  connection <- SQLite.open ":memory:"
  SQLite.execute_ connection
    (SQLite.Query $ T.pack "CREATE TABLE test (id INTEGER PRIMARY KEY, str text)")
  SQLite.execute connection (SQLite.Query $ T.pack "INSERT INTO test (str) VALUES (?)")
    (SQLite.Only ("value"))
  [(TestField id value)] <- SQLite.query_ connection (SQLite.Query $ T.pack "SELECT * FROM test") :: IO [TestField]
  "value" @=? value
  SQLite.close connection
