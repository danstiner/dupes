{-# LANGUAGE Rank2Types #-}

module Command.AddDupe (
	Options
  , parserInfo
  , run
) where

import Data.Machine hiding ( run )
import Control.Exception
import Control.Monad
import System.IO
import Data.Maybe ( catMaybes )
import Options.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory ( canonicalizePath, doesFileExist )
import System.FilePath ( (</>) )
import Control.DeepSeq
import Control.Monad.Trans.Class ( lift )
import qualified Database.LevelDB.Higher as Level

import Dupes
import qualified Settings
import Store.LevelDB as LevelDB

data CanonicalPath = Canonical FilePath | NonExistant FilePath

data Options = Options
  { optStdin :: Bool
  , optPaths :: [FilePath]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Add or update entries in the duplicate file index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "stdin"
     <> help "Read file names from STDIN" )
  <*> many
      ( argument str (metavar "PATH") )

instance DupesMonad IO

run :: Options -> IO ()
run opt = do
  appDir <- Settings.getAppDir
  let store = LevelDB.createStore (appDir </> "leveldb")
  runMachineIO machine store
  where
    machine = (fitM ioToDupes ioPart) ~> storePath
    ioToDupes = lift . lift
    ioPart = pathSource ~> checkPathP ~> processPath ~> catMaybesP
    pathSource = if (optStdin opt)
      then repeatedly $ lift getLine
      else source (optPaths opt)

runMachineIO :: SourceT (Dupes (Level.LevelDBT IO)) () -> Store -> IO ()
runMachineIO machine store = LevelDB.runDupes store $ runT_ machine

processPaths :: Options -> Store -> [CanonicalPath] -> IO ()
processPaths opt store checkedPaths = do
  toAdd <- mapM keyPair $ catCanonicals checkedPaths
  let toRemove = catNonExistants checkedPaths
  
  LevelDB.runDupes store $ do
    addAll $ catMaybes toAdd
    removeAll toRemove

checkPath :: FilePath -> IO CanonicalPath
checkPath path = do
  exists <- doesFileExist path
  if exists
    then (liftM Canonical (canonicalizePath path)) `catch` errorMessage
    else return nonExistant
  where
    errorMessage :: IOException -> IO CanonicalPath
    errorMessage ex = do
      putStrLn . ("error: " ++) $ show ex -- TODO Should be a logging statement
      return nonExistant
    nonExistant = NonExistant path

catCanonicals :: [CanonicalPath] -> [FilePath]
catCanonicals ps = [p | Canonical p <- ps]

catNonExistants :: [CanonicalPath] -> [FilePath]
catNonExistants ps = [p | NonExistant p <- ps]

keyPair :: FilePath -> IO (Maybe (FilePath, BucketKey))
keyPair path = do
  bucketKey <- calcBucketKey path
  case bucketKey of
    Nothing -> return Nothing
    Just key -> return $! Just (path, key)

calcBucketKey :: FilePath -> IO (Maybe BucketKey)
calcBucketKey path = calc `catch` errorMessage
  where
    calc = withBinaryFile path ReadMode $ \hnd -> do
      c <- L.hGetContents hnd
      let key = createBucketKeyLazy CRC32 c
      key `deepseq` (return $! Just key)
    errorMessage :: IOException -> IO (Maybe BucketKey)
    errorMessage ex = do
      putStrLn . ("error: " ++) $ show ex
      return Nothing

removeAll :: (DupesMonad m) => [FilePath] -> Dupes m ()
removeAll = mapM_ remove

addAll :: (DupesMonad m) => [(FilePath, BucketKey)] -> Dupes m ()
addAll = mapM_ (\(path, key) -> add path key)

checkPathP :: ProcessT IO FilePath CanonicalPath
checkPathP = repeatedly $ do
  await >>= lift . checkPath >>= yield

processPath :: ProcessT IO CanonicalPath (Maybe (FilePath, BucketKey))
processPath = repeatedly $ do
  path <- await
  case path of
    (Canonical p) -> yield =<< (lift $ keyPair p)
    otherwise -> return ()

catMaybesP :: Process (Maybe a) a
catMaybesP = repeatedly $ do
  m <- await
  case m of
    Just a -> yield a
    Nothing -> return () 

processBucket :: (DupesMonad m) => ProcessT (Dupes m) Bucket ()
processBucket = repeatedly $ do
  (Bucket key [(Entry path)]) <- await
  lift $ add path key

storePath :: (DupesMonad m) => ProcessT (Dupes m) (FilePath, BucketKey) ()
storePath = repeatedly $ do
  await >>= \(path, key) -> lift $ add path key
