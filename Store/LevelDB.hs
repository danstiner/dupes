{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Store.LevelDB (
	  createStore
	, Store
	, runLevelDBIndex
) where

import Index
import qualified Ref
import Store.Blob ()
import qualified Blob
import Store.Ref
import Data.ByteString
import Data.Binary as Binary
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)

import qualified Database.LevelDB.Higher as Level

newtype Store = Store FilePath

keySpace :: ByteString
keySpace = C.pack "MyKeySpace"

createStore :: FilePath -> Store
createStore path = Store path

runLevelDBIndex :: (MonadResourceBase m) => Index (Level.LevelDBT m) a -> Store -> m a
runLevelDBIndex m s = runLevelDB s (transformIndex m)

transformIndex :: (MonadResourceBase m) => Index (Level.LevelDBT m) a -> (Level.LevelDBT m) a
transformIndex actions = execIndex actions

runLevelDB :: (MonadResourceBase m) => Store -> Level.LevelDBT m a -> m a
runLevelDB store dbt = Level.runCreateLevelDB path keySpace dbt
	where (Store path) = store

instance (IndexMonad (Level.LevelDBT IO)) where
	set key value = lift $
		Level.put
			(toKeyIndex key)
			(L.toStrict $ Binary.encode value)

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
