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
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy         as L
import           Data.Monoid
import           Data.Serialize               as S
import           Data.Text                    as T
import           Data.Text.Encoding           as E
import           Database.LevelDB             as DB hiding (open)
import           Database.LevelDB.Streaming
import           Foreign.C.Types
import           Pipes
import           System.Posix.Files
import           System.Posix.Types

data Index = Index { getDb :: DB, getReadOptions :: ReadOptions, getWriteOptions :: WriteOptions, indexPath :: FilePath }

data IndexEntry = IndexEntry { indexEntryPath :: FilePath, indexEntryStat :: CachedStat }

data CachedStat = UnixCachedStat { mtime :: EpochTime, ctime :: EpochTime, inode :: FileID, size :: FileOffset, uid :: CUid, gid :: CGid } deriving (Eq, Ord)

data IndexChange
  = Insert FilePath
  | Remove FilePath
  | Unchanged FilePath
  deriving (Show)

instance Serialize CachedStat where
  put (UnixCachedStat (CTime mtime) (CTime ctime) (CIno inode) (COff size) (CUid uid) (CGid gid)) = do
    S.put mtime
    S.put ctime
    S.put inode
    S.put size
    S.put uid
    S.put gid
  get = do
    mtime <- S.get
    ctime <- S.get
    inode <- S.get
    size <- S.get
    uid <- S.get
    gid <- S.get
    return $ UnixCachedStat (CTime mtime) (CTime ctime) (CIno inode) (COff size) (CUid uid) (CGid gid)

open :: DB -> FilePath -> Index
open db = Index db defaultReadOptions defaultWriteOptions

update :: MonadResource m => Index -> Producer IndexChange m ()
update index = diff cmp (walk $ indexPath index) (list index) >-> update' index

cmp :: PathEntry -> IndexEntry -> Ordering
cmp (FileEntry filepath filestat) (IndexEntry indexpath indexstat) =
  compare filepath indexpath `mappend` compare (toCachedStat filestat) indexstat
cmp _ _ = LT

keyRange :: KeyRange
keyRange = KeyRange keyPrefix (compare keyPrefix)

keyPrefix :: B.ByteString
keyPrefix = B.singleton 0

toKey :: FilePath -> Key
toKey path = keyPrefix `B.append` E.encodeUtf8 (T.pack path)

fromKey :: Key -> FilePath
fromKey = T.unpack . E.decodeUtf8 . B.tail

toValue :: CachedStat -> Value
toValue = encode

fromValue :: Value -> CachedStat
fromValue v = case decode v of
  Left err -> assert False undefined
  Right stat -> stat

toCachedStat :: FileStatus -> CachedStat
toCachedStat s = UnixCachedStat (modificationTime s) (statusChangeTime s) (fileID s) (fileSize s) (fileOwner s) (fileGroup s)

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
    convert = forever $ await >>= \(key, value) -> yield (IndexEntry (fromKey key) (fromValue value))

update' :: (MonadResource m) => Index -> Pipe (SequenceDifference PathEntry IndexEntry) IndexChange m ()
update' (Index db _ writeOptions _) = forever $ await >>= go
  where
    go (LeftOnly a) = do
        lift $ DB.put db writeOptions (toKey $ getPath a) (toValue . toCachedStat $ getStatus a)
        yield $ Insert $ getPath a
    go (RightOnly b) = do
        lift $ DB.delete db writeOptions $ toKey $ indexEntryPath b
        yield $ Remove $ indexEntryPath b
    go (Common a _) = yield $ Unchanged $ getPath a
