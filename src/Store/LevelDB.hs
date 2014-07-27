{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Store.LevelDB (
    createStore
  , Store
  , runLevelDBIndex
  , runDupes
) where

import Dupes
import Index
import qualified Blob
import qualified Ref
import Store.Blob ()
import Store.Ref

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.ByteString
import Data.List (nub)
import qualified Control.Monad.Trans.State as State
import qualified Data.Binary as Binary
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Database.LevelDB.Higher as Level

newtype Store = Store FilePath
type KeySpace = ByteString

keySpace :: KeySpace
keySpace = C.pack "MyKeySpace"

indexKeySpace,dupesKeySpace :: KeySpace
indexKeySpace = C.pack "Index"
dupesKeySpace = C.pack "Dupes"

createStore :: FilePath -> Store
createStore = Store

runLevelDBIndex :: (MonadResourceBase m) => Index (Level.LevelDBT m) a -> Store -> m a
runLevelDBIndex m s = runLevelDB s indexKeySpace (execIndex m)

runDupes :: (MonadResourceBase m) => Store -> DupesT (Level.LevelDBT m) a -> m a
runDupes s m = runLevelDB s dupesKeySpace (execDupesT m)

runLevelDB :: (MonadResourceBase m) => Store -> KeySpace -> Level.LevelDBT m a -> m a
runLevelDB store =
	Level.runCreateLevelDB path
	where
		(Store path) = store

instance (IndexMonad (Level.LevelDBT IO)) where
	set key value = lift $
		Level.put
			(toKeyIndex key)
			(enc value)

enc :: (Binary.Binary a) => a -> C.ByteString
enc = L.toStrict . Binary.encode

decode :: (Binary.Binary a) => C.ByteString -> a
decode = Binary.decode . L.fromStrict

instance (DupesMonad (Level.LevelDBT IO)) where
	buckets bType = lift $ do
		items <- Level.scan (toLevelKey bType) Level.queryItems
		return $ Prelude.map decodeDupBucket items
	add path bucketKey = lift $ do
		let key = toLevelKey bucketKey
		existing <- Level.get key
		case existing of
			Just val -> do
				let prev = decode val :: [Dupes.Entry]
				Level.put key $ enc $ nub ((Entry path) : prev)
			Nothing  -> Level.put key $ enc [(Entry path)]
	remove path = lift $ do
		fail ("TODO, don't know how to remove path " ++ path)
	removeDir path = lift $ do
		fail ("TODO, don't know how to remove path " ++ path)

decodeDupBucket :: (Level.Key, Level.Value) -> Bucket
decodeDupBucket (key, val) = Bucket
	(decode key)
	(Binary.decode ( L.fromStrict val) :: [Dupes.Entry])

instance RefStore Store where
	read name = do
		(Store path) <- State.get
		Level.runCreateLevelDB path keySpace $ do
			r <- Level.get $ toKey name
			case r of
				Just val -> return $ Just $ Ref.Ref name (Binary.decode (L.fromStrict val) :: Blob.Id)
				Nothing -> return Nothing

	set (Ref.Ref name blobId) = do
		(Store path) <- State.get
		lift $ Level.runCreateLevelDB path keySpace $ do
			Level.put (toKey name) $ encodeStrict blobId
		return ()

	delete name = do
		(Store path) <- State.get
		lift $ Level.runCreateLevelDB path keySpace $ do
			Level.delete $ toKey name
		return ()

toKey :: Ref.Id -> Level.Key
toKey name = C.pack ("ref/" ++ (Ref.toString name))

toKeyIndex :: RelativeFilePath -> Level.Key
toKeyIndex path = C.pack ("index/" ++ path)

toLevelKey :: (Binary.Binary a) => a -> Level.Key
toLevelKey = encodeStrict

encodeStrict :: (Binary.Binary a) => a -> Level.Key
encodeStrict = L.toStrict . Binary.encode
