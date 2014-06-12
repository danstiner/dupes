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
import System.Directory ( canonicalizePath, doesFileExist, doesDirectoryExist )
import System.FilePath ( (</>) )
import Control.DeepSeq
import Control.Monad.Trans.Class ( lift )
import qualified Database.LevelDB.Higher as Level

import Dupes
import qualified Settings
import Store.LevelDB as LevelDB

data CanonicalPath = CanonicalFilePath FilePath | CanonicalDirectoryPath FilePath | NonExistant FilePath

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
      putStrLn . ("error: " ++) $ show ex
      return Nothing

checkPathP :: ProcessT IO FilePath CanonicalPath
checkPathP = repeatedly $ do
  await >>= lift . checkPath >>= yield

processPath :: ProcessT IO CanonicalPath (Maybe (FilePath, BucketKey))
processPath = repeatedly $ do
  path <- await
  case path of
    (CanonicalFilePath p) -> yield =<< (lift $ keyPair p)
    otherwise -> return ()

catMaybesP :: Process (Maybe a) a
catMaybesP = repeatedly $ do
  m <- await
  case m of
    Just a -> yield a
    Nothing -> return () 

storePath :: (DupesMonad m) => ProcessT (Dupes m) (FilePath, BucketKey) ()
storePath = repeatedly $ do
  await >>= \(path, key) -> lift $ add path key
