{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Store.LevelDB (
    createStore
  , Store
  , runLevelDBIndex
  , runDupes
  , runStoreOp
  , storeOpToDBAction
  , runDupesDBT
) where

import Dupes
import Index
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
import qualified Data.ByteString.Char8 as C
import qualified Database.LevelDB.Higher as Level
import System.FilePath ( (</>) )

newtype Store = Store { getStorePath :: FilePath }
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
runLevelDB = Level.runCreateLevelDB . getStorePath

instance RefStore Store where
  read name = do
    (Store path) <- State.get
    Level.runCreateLevelDB path keySpace $ do
      r <- Level.get $ toKey name
      case r of
        Just val -> let decoded = decode val in
          case decoded of
            Left _ -> return Nothing
            Right key -> return $ Just $ Ref.Ref name key
        Nothing -> return Nothing

  set (Ref.Ref name blobId) = do
    (Store path) <- State.get
    lift $ Level.runCreateLevelDB path keySpace $ do
      Level.put (toKey name) $ encode blobId
    return ()

  delete name = do
    (Store path) <- State.get
    lift $ Level.runCreateLevelDB path keySpace $ do
      Level.delete $ toKey name
    return ()

toKey :: Ref.Id -> Level.Key
toKey name = C.pack ("ref/" ++ (Ref.toString name))

runStoreOp :: StoreOp r -> IO r
runStoreOp = createDB dupesKeySpace . storeOpToDBAction

storeOpToDBAction :: StoreOp r -> Level.LevelDBT IO r
storeOpToDBAction (Pure r) = return r
storeOpToDBAction (Free (GetOp key f)) = fmap (emApply decode) (Level.get (encode key)) >>= storeOpToDBAction . f
storeOpToDBAction (Free (PutOp path key t)) = do
  Level.put (encode path) (encode path)
  Level.put ((C.pack "b/") `C.append` (encode key)) (encode [path])
  storeOpToDBAction t
storeOpToDBAction (Free (RmOp key t)) = (Level.delete (encode key)) >> storeOpToDBAction t
storeOpToDBAction (Free (ListOp prefix f)) = do
  items <- Level.withSnapshot $ Level.scan (encode prefix) Level.queryItems
  storeOpToDBAction . f . rights $ map (decode . fst) items
storeOpToDBAction (Free (BucketsOp f)) = do
  buckets <- Level.withSnapshot $ Level.scan (C.pack "b/") Level.queryItems
  storeOpToDBAction . f . rights $ map dec buckets
  where
    dec (k,v) = let b = decode (C.drop 2 k) :: Either String BucketKey in
      case b of
        Left e -> Left e
        Right key -> let ps = decode v :: Either String [PathKey] in
          case ps of
            Left e -> Left e
            Right paths -> Right $ Bucket key paths

runDupesDBT :: Level.LevelDBT IO r -> IO r
runDupesDBT = createDB dupesKeySpace

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
