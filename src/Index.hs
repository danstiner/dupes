{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Index (
    Index
  , update
  , open
  , list
  , IndexChange (..)
  , IndexEntry (..)
  , FileHash
) where

import           Logging

import           Data.Stream.Monadic.Pipes    as P
import           Pipes.Difference
import           Pipes.Path

import           Control.Applicative
import           Control.DeepSeq
import           Control.DeepSeq.Generics     (genericRnf)
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Crypto.Hash.SHA1             as SHA1
import qualified Data.ByteString              as B
import qualified Data.ByteString.Base16       as Base16
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as L
import           Data.Monoid
import           Data.Ord                     (comparing)
import           Data.Serialize               as S
import           Data.Text                    as T
import           Data.Text.Encoding           as E
import           Data.Text.Encoding.Error     as EE
import           Data.Word
import           Database.LevelDB             as DB hiding (open)
import           Database.LevelDB.Streaming
import           Foreign.C.Types
import           GHC.Generics                 (Generic)
import           Pipes
import           System.IO
import           System.IO.Unsafe             (unsafePerformIO)
import           System.Posix.Files
import           System.Posix.Types

logTag = "Index"

newtype FileHash = FileHash B.ByteString deriving (Eq, Ord, Generic)

instance NFData FileHash where rnf = genericRnf

data Index = Index { getDb :: DB, getReadOptions :: ReadOptions, getWriteOptions :: WriteOptions, indexPath :: FilePath }

data IndexEntry = IndexEntry { indexEntryPath :: FilePath, indexFileInfo :: FileInfo }

data FileInfo
  = UnixFileInfo { mtime :: EpochTime, ctime :: EpochTime, inode :: FileID, size :: FileOffset, uid :: CUid, gid :: CGid, getFileHash :: FileHash }
  deriving (Eq, Ord, Show)

data IndexChange
  = Insert FilePath FileHash
  | Remove FilePath FileHash
  | Unchanged FilePath FileHash
  deriving (Show)

instance Show FileHash where
  show (FileHash hash) = C.unpack $ Base16.encode hash

instance Serialize FileInfo where
  put (UnixFileInfo (CTime mtime) (CTime ctime) (CIno inode) (COff size) (CUid uid) (CGid gid) hash) = do
    S.put mtime
    S.put ctime
    S.put inode
    S.put size
    S.put uid
    S.put gid
    S.put hash
  get = do
    mtime <- S.get
    ctime <- S.get
    inode <- S.get
    size <- S.get
    uid <- S.get
    gid <- S.get
    hash <- S.get
    return $ UnixFileInfo (CTime mtime) (CTime ctime) (CIno inode) (COff size) (CUid uid) (CGid gid) hash

instance Serialize FileHash where
  put (FileHash hash) = S.put hash
  get = fmap FileHash S.get

open :: DB -> FilePath -> Index
open db = Index db defaultReadOptions defaultWriteOptions

update :: MonadResource m => Index -> Producer IndexChange m ()
update index = diff cmp (walk $ indexPath index) (list index) >-> update' index

cmp :: PathEntry -> IndexEntry -> Ordering
cmp (FileEntry filepath filestat) (IndexEntry indexpath indexstat) =
  compare filepath indexpath `mappend` compareIgnoringHash (toFileInfo' filestat) indexstat
cmp _ _ = LT

compareIgnoringHash :: FileInfo -> FileInfo -> Ordering
compareIgnoringHash = mconcat [comparing mtime, comparing ctime, comparing inode, comparing size, comparing uid, comparing gid]

keyRange :: KeyRange
keyRange = KeyRange keyPrefix (\k -> compare (B.head k) keyPrefixValue)

keyPrefix :: B.ByteString
keyPrefix = B.singleton keyPrefixValue

keyPrefixValue :: Word8
keyPrefixValue = 1

toKey :: FilePath -> Key
toKey path = keyPrefix `B.append` E.encodeUtf8 (T.pack path)

fromKey :: Key -> FilePath
fromKey key = T.unpack . E.decodeUtf8 . B.tail $ key

toValue :: FileInfo -> Value
toValue = encode

fromValue :: Value -> FileInfo
fromValue v = case decode v of
  Left err -> assert False undefined
  Right stat -> stat

toFileInfo :: FileStatus -> FileHash -> FileInfo
toFileInfo s hash = UnixFileInfo (modificationTime s) (statusChangeTime s) (fileID s) (fileSize s) (fileOwner s) (fileGroup s) hash

toFileInfo' :: FileStatus -> FileInfo
toFileInfo' s = toFileInfo s nullHash
  where
    nullHash = FileHash B.empty

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
        result <- liftIO $ hashFile (getPath a)
        case result of
          Left ex -> liftIO $ errorM logTag ex
          Right hash -> do
            lift $ DB.put db writeOptions (toKey $ getPath a) (toValue $ toFileInfo (getStatus a) hash)
            yield $ Insert (getPath a) hash
    go (RightOnly b) = do
        lift $ DB.delete db writeOptions $ toKey $ indexEntryPath b
        yield $ Remove (indexEntryPath b) (getFileHash $ indexFileInfo b)
    go (Common _ b) = yield $ Unchanged (indexEntryPath b) (getFileHash $ indexFileInfo b)

hashFile :: MonadIO m => FilePath -> m (Either String FileHash)
hashFile path = liftIO (calc `catch` errorMessage)
  where
    calc = withBinaryFile path ReadMode $ \hnd -> do
      c <- L.hGetContents hnd
      let key = calcHash c
      key `deepseq` return (Right key)
    errorMessage :: IOException -> IO (Either String FileHash)
    errorMessage = return . Left . show
    calcHash c = FileHash $! SHA1.hashlazy c
