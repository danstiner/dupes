{-# LANGUAGE RankNTypes #-}

module Pipes.SQLite.Simple (query) where

import Pipes
import Pipes.Safe

import           Database.SQLite.Simple.Types
import Database.SQLite.Simple hiding (withStatement, query)

-- | Perform a SQL query in a streaming manner, calling 'nextRow'
-- each time the 'Producer' is 'await'ed  
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string mismatched with given arguments.
--
-- * 'ResultError': result conversion failed.
query :: ( FromRow row, ToRow params )
        => Connection
        -> Query
        -> params
        -> Producer row (SafeT IO) ()
query conn query params = withStatement conn query $ \stmt -> do
  lift . lift $ bind stmt (toRow params)
  doQuery stmt

-- | Opens a prepared statement, executes an action using this statement, and
-- closes the statement after the 'Producer' completes, even in the presence of exceptions.
withStatement :: Connection -> Query -> (Statement -> Producer a (SafeT IO) ()) -> Producer a (SafeT IO) ()
withStatement conn query = bracket (openStatement conn query) closeStatement

doQuery :: ( FromRow row )
          => Statement
          -> Producer row (SafeT IO) ()
doQuery stmt =
    go
  where
    go = do
      maybeNextRow <- lift . lift $ nextRow stmt
      case maybeNextRow of
        Just row -> do
          yield row
          go
        Nothing -> return ()
