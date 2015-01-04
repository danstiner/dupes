{-# LANGUAGE RankNTypes #-}

module Index (
    Index
  , update
  , open
  , list
  , IndexChange (..)
  , IndexEntry (..)
) where

import           Data.Stream.Monadic.Pipes    as P
import           Pipes.Difference
import           Pipes.Path

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import           Database.LevelDB             as DB hiding (open)
import           Database.LevelDB.Streaming
import           Pipes

data Index = Index { db ::DB, getReadOptions :: ReadOptions, getWriteOptions :: WriteOptions, indexPath :: FilePath }

data IndexEntry = IndexEntry { indexEntryPath :: FilePath }

data IndexChange
  = Insert FilePath
  | Remove FilePath
  | Unchanged FilePath
  deriving (Show)

open :: DB -> FilePath -> Index
open db = Index db defaultReadOptions defaultWriteOptions

update :: MonadResource m => Index -> Producer IndexChange m ()
update index = diff cmp (walk $ indexPath index) (list index) >-> update' index

cmp :: PathEntry -> IndexEntry -> Ordering
cmp (FileEntry filepath _) (IndexEntry indexpath) = compare filepath indexpath
cmp _ _ = LT

keyRange :: KeyRange
keyRange = AllKeys -- KeyRange keyPrefix (const GT) --cmp . compare keyPrefix)
  where
    cmp EQ = LT
    cmp a = a

keyPrefix = B.singleton 0

toKey :: FilePath -> Key
toKey path = keyPrefix `B.append` C.pack path

fromKey :: Key -> FilePath
fromKey = C.unpack . B.tail

producerFromIterator :: MonadResource m => DB -> ReadOptions -> (Iterator -> Producer a m ()) -> Producer a m ()
producerFromIterator db opts f = do
    (rk, iter) <- lift $ iterOpen' db opts
    res <- f iter
    lift $ release rk
    return res

list :: MonadResource m => Index -> Producer IndexEntry m ()
list (Index db readOptions _ _) = producerFromIterator db readOptions go
  where
    go :: (Applicative m, MonadIO m) => Iterator -> Producer IndexEntry m ()
    go iterator = P.fromStream (entrySlice iterator keyRange Asc) >-> convert
    convert :: (Monad m) => Pipe Entry IndexEntry m ()
    convert = forever $ await >>= \(key, value) -> yield (IndexEntry (fromKey key))

update' :: (MonadResource m) => Index -> Pipe (SequenceDifference PathEntry IndexEntry) IndexChange m ()
update' (Index db _ writeOptions _) = forever $ await >>= go
  where
    go (LeftOnly a) = do
        lift $ DB.put db writeOptions (toKey $ getPath a) B.empty
        yield $ Insert $ getPath a
    go (RightOnly b) = do
        lift $ DB.delete db writeOptions $ toKey $ indexEntryPath b
        yield $ Remove $ indexEntryPath b
    go (Common a b) = yield $ Unchanged $ getPath a
