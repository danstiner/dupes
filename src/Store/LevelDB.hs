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
  , DupesPathLevelKey (..)
) where

import Dupes
import Index
import qualified Ref
import qualified Settings
import Store.Blob ()
import Store.Ref

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Free
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (MonadResourceBase)
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Serialize (Serialize,encode,decode)
import Data.Set (Set)
import qualified Control.Monad.Trans.State as State
import qualified Data.ByteString.Char8 as C
import qualified Data.Serialize as Serialize
import qualified Data.Set as Set
import qualified Database.LevelDB.Higher as Level
import System.FilePath ( (</>) )

newtype Store = Store { getStorePath :: FilePath }
type KeySpace = ByteString

newtype DupesPathLevelKey = DupesPathLevelKey FilePath deriving (Eq, Ord, Show)

instance Serialize DupesPathLevelKey where
  put (DupesPathLevelKey path) = Serialize.putByteString (C.pack path)
  get = liftM DupesPathLevelKey $ fmap C.unpack $ Serialize.remaining >>= Serialize.getByteString

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
storeOpToDBAction (Free (GetOp path f)) = fmap (emApply decode) (Level.get (encodePathKey path)) >>= storeOpToDBAction . f
storeOpToDBAction (Free (PutOp path key t)) = do
  let encPath = encodePathKey path
      encKey  = (C.pack "b/") `C.append` (encode key)
  Level.put encPath (encode path)
  existing <- Level.get encKey
  Level.put encKey . encode $ case existing of
    Nothing -> Set.singleton path
    Just b  -> let dec = decode b :: Either String (Set PathKey) in
      case dec of
        Left _ -> Set.singleton path
        Right paths -> Set.insert path paths
  storeOpToDBAction t
storeOpToDBAction (Free (RmOp path t)) = (Level.delete (encodePathKey path)) >> storeOpToDBAction t
storeOpToDBAction (Free (ListOp prefix f)) = do
  items <- Level.withSnapshot $ Level.scan (encodePathKey prefix) Level.queryItems
  storeOpToDBAction . f . rights $ map (decodePathKey . fst) items
storeOpToDBAction (Free (BucketsOp f)) = do
  buckets <- Level.withSnapshot $ Level.scan (C.pack "b/") Level.queryItems
  storeOpToDBAction . f . rights $ map dec buckets
  where
    dec (k,v) = let b = decode (C.drop 2 k) in
      case b of
        Left e -> Left e
        Right key -> let ps = decode v in
          case ps of
            Left e -> Left e
            Right paths -> Right $ Bucket key paths

encodePathKey :: PathKey -> Level.Key
encodePathKey = encode . DupesPathLevelKey . unPathKey

decodePathKey :: Level.Key -> Either String PathKey
decodePathKey key = case decode key of
  Left a -> Left a
  Right (DupesPathLevelKey path) -> Right (toPathKey path)

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
