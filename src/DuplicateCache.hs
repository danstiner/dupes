{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DuplicateCache (
    DuplicateCache (..)
    , HashPath (..)
    , update
    , open
    , list
) where

import           Index                        (FileHash, IndexChange (..))
import           Logging

import           Data.Stream.Monadic.Pipes    as P

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import           Data.Either
import           Data.Int
import           Data.Maybe
import           Data.Serialize
import           Data.Serialize               as S
import           Data.Text                    as T
import           Data.Text.Encoding           as E
import           Data.Word
import           Database.LevelDB             as DB hiding (open)
import           Database.LevelDB.Streaming   as DBS
import           Pipes

data DuplicateCache = DuplicateCache { getDb :: DB, getReadOptions :: ReadOptions, getWriteOptions :: WriteOptions }

data HashPath = HashPath { getFileHash :: !FileHash, getFilePath :: !FilePath }

data DupeList = None | ZeroDupe !FilePath | ManyDupe  deriving (Show)

instance Serialize HashPath where
  put (HashPath hash path) = S.put hash >> S.put path
  get = liftA2 HashPath S.get S.get

logTag = "DuplicateCache"

open :: DB -> DuplicateCache
open db = DuplicateCache db defaultReadOptions defaultWriteOptions

update :: MonadResource m => DuplicateCache -> Consumer IndexChange m ()
update cache = forever $ await >>= go
  where
    go (Insert path hash) = lift $ add cache path hash
    go (Remove path hash) = lift $ remove cache path hash
    go _ = return ()

add :: MonadResource m => DuplicateCache -> FilePath -> FileHash -> m ()
add cache@(DuplicateCache db _ writeOptions) path hash = do
    count <- dupeList cache hash
    add'
    liftIO $ putStr "+ " >> putStrLn path
    case count of
      None -> return ()
      (ZeroDupe otherPath) -> addPath cache path hash >> addPath cache otherPath hash
      ManyDupe -> addPath cache path hash
  where
    add' = DB.put db writeOptions (toHashPathKey $ HashPath hash path) defaultPathValue

remove :: (MonadResource m) => DuplicateCache -> FilePath -> FileHash -> m ()
remove cache path hash = do
    remove' cache
    count <- dupeList cache hash
    liftIO $ putStr "- " >> putStrLn path
    case count of
      None -> return ()
      (ZeroDupe otherPath) -> removePath cache path >> removePath cache otherPath
      ManyDupe -> removePath cache path
  where
    remove' (DuplicateCache db _ writeOptions) = DB.delete db writeOptions (toHashPathKey $ HashPath hash path)

addPath :: MonadResource m => DuplicateCache -> FilePath -> FileHash -> m ()
addPath (DuplicateCache db _ writeOptions) path hash = DB.put db writeOptions (toPathKey path) (toPathValue hash)

removePath :: MonadResource m => DuplicateCache -> FilePath -> m ()
removePath (DuplicateCache db _ writeOptions) path = DB.delete db writeOptions (toPathKey path)

producerFromIterator :: MonadResource m => DB -> ReadOptions -> (Iterator -> Producer a m ()) -> Producer a m ()
producerFromIterator db opts f = do
    (rk, iter) <- lift $ iterOpen' db opts
    res <- f iter
    lift $ release rk
    return res

list :: MonadResource m => DuplicateCache -> Producer HashPath m ()
list (DuplicateCache db readOptions _) = producerFromIterator db readOptions go
  where
    go :: (Applicative m, MonadIO m) => Iterator -> Producer HashPath m ()
    go iterator = P.fromStream (entrySlice iterator pathKeyRange Asc) >-> convert
    convert :: (Monad m) => Pipe Entry HashPath m ()
    convert = forever $ await >>= \(key, value) -> yield (HashPath (fromPathValue value) (fromPathKey key))

dupeList :: MonadResource m => DuplicateCache -> FileHash -> m DupeList
dupeList (DuplicateCache db readOptions _) hash =
  withIterator db readOptions $ \iterator -> do
  first2 <- toList . DBS.take 2 $ keySlice iterator (hashKeyRange hash) Asc
  case first2 of
    [] -> return None
    [key] -> return $ either (const None) (ZeroDupe . getFilePath) (fromHashPathKey key)
    _ -> return ManyDupe

zeroToNothing :: (Eq a, Num a) => a -> Maybe a
zeroToNothing 0 = Nothing
zeroToNothing a = Just a

nothingToZero :: (Num a) => Maybe a -> a
nothingToZero = fromMaybe 0

pathKeyPrefix :: B.ByteString
pathKeyPrefix = B.singleton pathKeyPrefixValue

pathKeyPrefixValue :: Word8
pathKeyPrefixValue = 2

pathKeyRange :: KeyRange
pathKeyRange = KeyRange pathKeyPrefix (\k -> compare (B.head k) pathKeyPrefixValue)

toPathKey :: FilePath -> Key
toPathKey path = pathKeyPrefix `B.append` encode path

fromPathKey :: Key -> FilePath
fromPathKey = either (assert False undefined) id . decode . B.tail

toPathValue :: FileHash -> Value
toPathValue = encode

fromPathValue :: Value -> FileHash
fromPathValue = either (assert False undefined) id . decode

hashKeyRange :: FileHash -> KeyRange
hashKeyRange hash = KeyRange hashPrefix hashPrefixCompare
  where
    hashPrefix = hashPathKeyPrefix `B.append` encode hash
    hashPrefixCompare v = compare (B.take (B.length hashPrefix) v) hashPrefix

hashPathKeyPrefix :: B.ByteString
hashPathKeyPrefix = B.singleton hashPathKeyPrefixValue

hashPathKeyPrefixValue :: Word8
hashPathKeyPrefixValue = 3

toHashPathKey :: HashPath -> Key
toHashPathKey = (hashPathKeyPrefix `B.append`) . encode

fromHashPathKey :: Key -> Either String HashPath
fromHashPathKey = decode . B.tail

defaultPathValue :: Value
defaultPathValue = B.empty
