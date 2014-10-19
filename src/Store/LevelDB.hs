{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Store.LevelDB (
    runLevelDBIndex
  , runDupes
  , runStoreOp
  , storeOpToDBAction
  , runDupesDBT
  , DupesPathLevelKey (..)
) where

import Dupes
import Index
import Logging
import qualified App
import Store.Blob ()
import Store.Repository

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Serialize (Serialize,encode,decode)
import qualified Data.ByteString.Char8 as C
import qualified Data.List as List
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import qualified Database.LevelDB.Higher as Level
import System.Log.Logger

type KeySpace = ByteString

data DupeLevelKey = DupeLevelKey BucketKey PathKey

newtype DupesPathLevelKey = DupesPathLevelKey FilePath deriving (Eq, Ord, Show)

instance Serialize DupesPathLevelKey where
  put (DupesPathLevelKey path) = Serialize.putByteString (C.pack path)
  get = liftM DupesPathLevelKey $ fmap C.unpack (Serialize.remaining >>= Serialize.getByteString)

instance Serialize DupeLevelKey where
  put (DupeLevelKey bucketKey pathKey) = do
    Serialize.put bucketKey
    Serialize.put pathKey
  get = liftA2 DupeLevelKey Serialize.get Serialize.get

logTag :: String
logTag = App.logTag ++ ".Store.LevelDB"

indexKeySpace,dupesKeySpace :: KeySpace
indexKeySpace = C.pack "Index"
dupesKeySpace = C.pack "Dupes"

dupePathKeySpace,dupeBucketKeySpace,dupeDupesKeySpace :: KeySpace
dupePathKeySpace = C.pack "Path"
dupeBucketKeySpace = C.pack "Bucket"
dupeDupesKeySpace = C.pack "Dupe"

dupeBucketPrefix :: ByteString
dupeBucketPrefix = C.empty

runLevelDBIndex :: (MonadResourceBase m) => Index (Level.LevelDBT m) a -> Store -> m a
runLevelDBIndex m s = runLevelDB s indexKeySpace (execIndex m)

runDupes :: (MonadResourceBase m) => Store -> DupesT (Level.LevelDBT m) a -> m a
runDupes s m = runLevelDB s dupesKeySpace (execDupesT m)

runLevelDB :: (MonadResourceBase m) => Store -> KeySpace -> Level.LevelDBT m a -> m a
runLevelDB = Level.runCreateLevelDB . getStorePath

runStoreOp :: Store -> StoreOp r -> IO r
runStoreOp store = runDupesDBT store . storeOpToDBAction

runDupesDBT :: Store -> Level.LevelDBT IO r -> IO r
runDupesDBT (Store path) = createDB path dupesKeySpace

storeOpToDBAction :: StoreOp r -> Level.LevelDBT IO r
storeOpToDBAction (Pure r) = return r
storeOpToDBAction (Free (GetOp path f)) = Level.withKeySpace dupePathKeySpace $
  Level.get (encodePathKey path) >>= storeOpToDBAction . f . (const path <$>)
storeOpToDBAction (Free (PutOp path key t)) = do
  existing <- Level.withKeySpace dupeBucketKeySpace $ Level.get encKey
  case existing of
    Nothing -> Level.withKeySpace dupeBucketKeySpace $ Level.put encKey encPath
    Just prev -> addDupeEntry prev
  Level.withKeySpace dupePathKeySpace $ Level.put encPath encKey
  storeOpToDBAction t
  where
    encPath = encodePathKey path
    encKey  = encodeSingletonBucketKey key
    encDupeKey = encodeDupeBucketKey key path
    addDupeEntry prevVal = Level.withKeySpace dupeDupesKeySpace $ do
      case (decodePathKey prevVal) of
        Left msg -> lift $ errorM logTag msg >> errorM logTag (show prevVal)
        Right prevPath -> Level.put (encodeDupeBucketKey key prevPath) C.empty
      Level.put encDupeKey C.empty

storeOpToDBAction (Free (RmOp path t)) = (Level.delete (encodePathKey path)) >> storeOpToDBAction t
storeOpToDBAction (Free (ListOp prefix f)) = do
  items <- Level.withKeySpace dupePathKeySpace $ Level.scan (encodePathKey prefix) Level.queryItems
  storeOpToDBAction . f . rights $ map (decodePathKey . fst) items
storeOpToDBAction (Free (DupesOp f)) =
  scanDupeBuckets >>= storeOpToDBAction . f

scanDupeBuckets :: Level.LevelDBT IO [Bucket]
scanDupeBuckets = do
  dupeItems <- Level.withKeySpace dupeDupesKeySpace $ Level.scan dupeBucketPrefix Level.queryItems
  let dupeDecodedEntries = map decodeDupeBucketEntry dupeItems
  dupeEntries <- lift $ logLefts logTag WARNING dupeDecodedEntries
  return $ map toBucket $ List.groupBy (\a b -> fst a == fst b) dupeEntries
  where
    toBucket xs = Bucket (fst $ head xs) (Set.fromList $ map snd xs)

encodeSingletonBucketKey :: BucketKey -> Level.Key
encodeSingletonBucketKey = encode

encodeDupeBucketKey :: BucketKey -> PathKey -> Level.Key
encodeDupeBucketKey bucket path =
  encode $ DupeLevelKey bucket path

decodeDupeBucketEntry :: (Level.Key, Level.Value) -> Either String (BucketKey, PathKey)
decodeDupeBucketEntry (key, _) =
  unpack <$> decode key
  where
    unpack (DupeLevelKey bucketKey pathKey) = (bucketKey, pathKey)

encodePathKey :: PathKey -> Level.Key
encodePathKey = encode . DupesPathLevelKey . unPathKey

decodePathKey :: Level.Key -> Either String PathKey
decodePathKey key = case decode key of
  Left a -> Left a
  Right (DupesPathLevelKey path) -> Right (toPathKey path)

createDB :: FilePath -> KeySpace -> Level.LevelDBT IO a -> IO a
createDB = Level.runCreateLevelDB
