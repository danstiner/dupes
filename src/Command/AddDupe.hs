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
import Options.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory ( canonicalizePath, doesFileExist, doesDirectoryExist, getDirectoryContents )
import Control.DeepSeq
import Control.Monad.Trans.Class ( lift )
import qualified Database.LevelDB.Higher as Level
import Data.Maybe (catMaybes)
import System.Log.Logger

import Dupes
import Util
import Store.LevelDB as LevelDB

data CanonicalPath =
    CanonicalFilePath FilePath
  | CanonicalDirectoryPath FilePath
  | NonExistant FilePath

type DirectoryContents = (CanonicalPath, [CanonicalPath])
type PathHash = (CanonicalPath, BucketKey)
type HashedDirectoryContents = (PathHash, [PathHash])

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

run :: Options -> IO ()
run opt = do
  store <- getStore
  runMachineIO machine store
  where
    machine = (fitM ioToDupes ioMachine) ~> storeDirectory
    ioMachine = pathSource ~> canonicalize ~> recurseDirC ~> hashDir
    ioToDupes = lift . lift
    pathSource = if (optStdin opt)
      then repeatedly $ lift getLine
      else source (optPaths opt)

canonicalize :: ProcessT IO FilePath CanonicalPath
canonicalize = repeatedly $ do
  await >>= (lift . checkPath) >>= yield

recurseDirC :: ProcessT IO CanonicalPath DirectoryContents
recurseDirC = repeatedly $ do
  a <- await
  entries <- lift $ process a
  yield (a, entries)
  where
    process (CanonicalDirectoryPath dir) = do
      contents <- getDirectoryContents dir `catch` ioException
      entries <- mapM checkPath contents
      return entries
    process (CanonicalFilePath _) = return []
    process (NonExistant _) = return []
    ioException :: IOException -> IO [FilePath]
    ioException ex = do
      warningM "" $ ("warn: " ++) $ show ex
      return []

hashDir :: ProcessT IO DirectoryContents HashedDirectoryContents
hashDir = repeatedly $ do
  (dirPath, entries) <- await
  maybeDirHash <- lift $ hash dirPath
  case maybeDirHash of
    Just dirHash -> do
      hashedEntries <- lift $ mapM process entries
      yield ((dirPath, dirHash), catMaybes hashedEntries)
    Nothing -> return ()
  where
    process entry = do
      h <- hash entry
      return $ liftM (\h -> (entry, h)) h
    hash (CanonicalFilePath path) = calcBucketKey path
    hash (CanonicalDirectoryPath _) = return $ Just nilBucketKey
    hash (NonExistant _) = return $ Just nilBucketKey

storeDirectory :: (DupesMonad m) => ProcessT (DupesT m) HashedDirectoryContents ()
storeDirectory = repeatedly $ do
  (path, entries) <- await
  process path entries
  where
    --process :: (DupesMonad m) => PathHash -> [PathHash] -> PlanT? (Dupes m) ()
    process ((CanonicalDirectoryPath dir), _) entries = lift $ do
      removeDir dir
      mapM_ (\(path, key) -> addC path key) entries
      return ()
    process ((CanonicalFilePath path), key) _ = lift $ add path key
    process ((NonExistant _), _) _ = return ()
    addC (CanonicalFilePath path) key = add path key
    addC _ _ = return ()

runMachineIO :: SourceT (DupesT (Level.LevelDBT IO)) () -> Store -> IO ()
runMachineIO machine store = LevelDB.runDupes store $ runT_ machine

checkPath :: FilePath -> IO CanonicalPath
checkPath path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  checkPathHelper path isFile isDirectory

checkPathHelper :: FilePath -> Bool -> Bool -> IO CanonicalPath
checkPathHelper path isFile isDirectory
  | isFile && isDirectory = fail "Not possible"
  | isFile = (liftM CanonicalFilePath (canonicalizePath path)) `catch` ioException
  | isDirectory = (liftM CanonicalDirectoryPath (canonicalizePath path)) `catch` ioException
  | otherwise = return nonExistant
  where
    ioException :: IOException -> IO CanonicalPath
    ioException ex = do
      putStrLn . ("error: " ++) $ show ex -- TODO Should be a logging statement
      return nonExistant
    nonExistant = NonExistant path

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
      putStrLn . ("errorc: " ++) $ show ex
      return Nothing
