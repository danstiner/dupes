module Dupes.Index (IndexPath, construct) where

import           Control.Monad.IO.Class
import           Database.SQLite        as SQLite

data Index = Index SQLite.Connection

newtype IndexPath = IndexPath FilePath
  deriving Show

construct :: FilePath -> IndexPath
construct = IndexPath

withIndex :: MonadIO m => IndexPath -> (Index -> m a) -> m a
withIndex (IndexPath path) f = SQLite.with path (f . Index)

updateFile :: Index -> FilePath -> IO ()
updateFile (Index connection) = undefined
