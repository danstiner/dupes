{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Pipes.SQLite.Simple (query, query_, integrationTests) where

import           Pipes
import qualified Pipes.Prelude                as P
import           Pipes.Safe

import qualified Data.Text                    as T
import           Database.SQLite.Simple       (Connection, FromRow, Statement,
                                               ToRow)
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types

import           Test.QuickCheck.Monadic
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

-- | Perform a SQL query in a streaming manner, calling 'nextRow' each time the 'Producer' is
-- 'await'ed
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string mismatched with given arguments.
--
-- * 'ResultError': result conversion failed.
query :: (FromRow row, ToRow params)
      => Connection
      -> Query
      -> params
      -> Producer row (SafeT IO) ()
query conn query params = withStatement conn query $ \stmt -> do
  lift . lift $ SQLite.bind stmt (SQLite.toRow params)
  doQuery stmt

-- | Perform a SQL query in a streaming manner, calling 'nextRow' each time the 'Producer' is
-- 'await'ed
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string mismatched with given arguments.
--
-- * 'ResultError': result conversion failed.
query_ :: (FromRow row)
       => Connection
       -> Query
       -> Producer row (SafeT IO) ()
query_ conn query = withStatement conn query doQuery

-- | Opens a prepared statement, executes an action using this statement, and closes the statement
-- after the 'Producer' completes, even in the presence of exceptions.
withStatement :: Connection -> Query -> (Statement -> Producer a (SafeT IO) ()) -> Producer a (SafeT IO) ()
withStatement conn query = bracket (SQLite.openStatement conn query) SQLite.closeStatement

doQuery :: (FromRow row)
        => Statement
        -> Producer row (SafeT IO) ()
doQuery stmt =
  go
  where
    go = do
      maybeNextRow <- lift . lift $ SQLite.nextRow stmt
      case maybeNextRow of
        Just row -> do
          yield row
          go
        Nothing -> return ()

data TestField = TestField { i_ :: Int }
  deriving Show

instance FromRow TestField where
  fromRow = TestField <$> SQLite.field

instance ToRow TestField where
  toRow (TestField i) = SQLite.toRow (SQLite.Only i)

prop_query_produces_correct_rows_in_order :: [Int] -> Property
prop_query_produces_correct_rows_in_order xs = monadicIO $ do
  xs' <- run $ SQLite.withConnection ":memory:" $ \connection -> do
           SQLite.execute_ connection "CREATE TABLE test (i INTEGER)"
           mapM_ (SQLite.execute connection "INSERT INTO test (i) VALUES (?)" . TestField) xs
           runSafeT $ P.toListM (query_ connection "SELECT i FROM test")
  assert (xs == map i_ xs')

integrationTests = $(testGroupGenerator)
