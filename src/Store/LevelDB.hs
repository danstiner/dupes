{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Store.LevelDB (
    createStore
  , Store
  , runLevelDBIndex
  , runDupes
  , runStoreOp
) where

import Dupes
import Index
import qualified Blob
import qualified Ref
import qualified Settings
import Store.Blob ()
import Store.Ref

import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Serialize (encode,decode)
import qualified Control.Monad.Trans.State as State
import qualified Data.Binary as Binary
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Database.LevelDB.Higher as Level
import System.FilePath ( (</>) )

newtype Store = Store FilePath
type KeySpace = ByteString

keySpace,indexKeySpace,dupesKeySpace :: KeySpace
keySpace = C.pack "MyKeySpace"
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

encodeStrict :: (Binary.Binary a) => a -> Level.Key
encodeStrict = L.toStrict . Binary.encode

runStoreOp :: StoreOp r -> IO r
runStoreOp = createDB dupesKeySpace . runStoreOp'

runStoreOp' :: StoreOp r -> Level.LevelDBT IO r
runStoreOp' (Pure r) = return r
runStoreOp' (Free (GetOp key g)) = fmap (emApply decode) (Level.get (encode key)) >>= runStoreOp' . g
runStoreOp' (Free (PutOp key t)) = lift (putStr "Add: ") >> lift (putStrLn (show key)) >> (Level.put (encode key) (encode key)) >> runStoreOp' t
runStoreOp' (Free (RmOp key t)) = (Level.delete (encode key)) >> runStoreOp' t
runStoreOp' (Free (ListOp prefix g)) = do
  items <- Level.withSnapshot $ Level.scan (encode prefix) Level.queryItems
  runStoreOp' . g . rights $ map (decode . fst) items

createDB :: KeySpace -> Level.LevelDBT IO a -> IO a
createDB space actions = do
  appDir <- Settings.getAppDir
  Level.runCreateLevelDB (appDir </> "leveldb") space actions

emApply :: (a -> Either e b) -> Maybe a -> Maybe b
emApply f (Just a) = maybeSnd (f a)
emApply _ Nothing = Nothing

maybeSnd :: Either a b -> Maybe b
maybeSnd (Left _) = Nothing
maybeSnd (Right b) = Just b
