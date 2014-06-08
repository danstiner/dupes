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
import qualified Dupes
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

indexKeySpace :: KeySpace
indexKeySpace = C.pack "Index"

dupesKeySpace :: KeySpace
dupesKeySpace = C.pack "Dupes"

createStore :: FilePath -> Store
createStore path = Store path

runLevelDBIndex :: (MonadResourceBase m) => Index (Level.LevelDBT m) a -> Store -> m a
runLevelDBIndex m s = runLevelDB s indexKeySpace (execIndex m)

runDupes :: (MonadResourceBase m) => Store -> Dupes (Level.LevelDBT m) a -> m a
runDupes s m = runLevelDB s dupesKeySpace (execDupes m)

runLevelDB :: (MonadResourceBase m) => Store -> KeySpace -> Level.LevelDBT m a -> m a
runLevelDB store keySpace dbt =
	Level.runCreateLevelDB path keySpace dbt
	where
		(Store path) = store

instance (IndexMonad (Level.LevelDBT IO)) where
	set key value = lift $
		Level.put
			(toKeyIndex key)
			(enc value)

--put :: (Monad m, Binary.Binary a) => String-> a -> m ()
--put k v = lift $ Level.put (C.pack k) (enc v)

enc :: (Binary.Binary a) => a -> C.ByteString
enc = L.toStrict . Binary.encode

decode :: (Binary.Binary a) => C.ByteString -> a
decode = Binary.decode . L.fromStrict

instance (DupesMonad (Level.LevelDBT IO)) where
	list bType = lift $ do
		items <- Level.scan (toLeveKeyPrefixDupes bType) Level.queryItems
		return $ Prelude.map decodeDupBucket items
	add path bKey = lift $ do
		let key = toLevelKeyDupes bKey
		existing <- Level.get key
		case existing of
			Just val -> do
				let prev = decode val :: [Dupes.Entry]
				Level.put key $ enc $ nub ((Entry path) : prev)
			Nothing  -> Level.put key $ enc [(Entry path)]
	rm path = lift $ do
		fail "TODO"

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
		return Nothing

	set (Ref.Ref name blobId) = do
		(Store path) <- State.get
		lift $ Level.runCreateLevelDB path keySpace $ do
			Level.put (toKey name) $ L.toStrict $ Binary.encode blobId
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

toLevelKeyDupes :: Dupes.Key -> Level.Key
toLevelKeyDupes = L.toStrict . Binary.encode

toLeveKeyPrefixDupes :: Dupes.BucketType -> Level.Key
toLeveKeyPrefixDupes = L.toStrict . Binary.encode
