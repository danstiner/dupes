
module Store.LevelDB (
	createStore
	, Store
) where

import Ref
import Store.Blob
import qualified Blob
import Store.Ref
import Data.ByteString
import Data.Binary as Binary
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)

import qualified Database.LevelDB.Higher as Level

newtype Store = Store FilePath

keySpace :: ByteString
keySpace = C.pack "MyKeySpace"

createStore :: FilePath -> Store
createStore path = Store path

instance RefStore Store where
	read name = do
		(Store path) <- State.get
		ref <- Level.runCreateLevelDB path keySpace $ do
			r <- Level.get $ toKey name
			case r of
				Just val -> return $ Just $ Ref name (Binary.decode (L.fromStrict val) :: Blob.Id)
		return Nothing

	set (Ref name blobId) = do
		(Store path) <- State.get
		lift $ Level.runCreateLevelDB path keySpace $ do
			Level.put (toKey name) $ L.toStrict  $ Binary.encode blobId
		return ()

	delete name = do
		(Store path) <- State.get
		lift $ Level.runCreateLevelDB path keySpace $ do
			Level.delete $ toKey name
		return ()

toKey :: Id -> Level.Key
toKey name = C.pack ("ref:" ++ (toString name))