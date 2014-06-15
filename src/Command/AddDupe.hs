module Command.AddDupe (
	Options
  , parserInfo
  , run
) where

import Data.Maybe ()
import Control.Exception
import Control.Monad
import System.IO
import Data.Maybe ( catMaybes )
import Options.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory ( canonicalizePath, doesFileExist )
import System.FilePath ( (</>) )
import Control.DeepSeq

import Dupes
import qualified Settings
import Store.LevelDB as LevelDB

data CheckedPath = Canonical FilePath | NonExistant FilePath

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

  processPaths opt store =<< mapM checkPath =<< getPaths

  where
    getPaths = if (optStdin opt)
      then readPaths
      else return (optPaths opt)
    readPaths = liftM lines getContents

processPaths :: Options -> Store -> [CheckedPath] -> IO ()
processPaths opt store checkedPaths = do
  toAdd <- mapM keyPair $ catCanonicals checkedPaths
  let toRemove = catNonExistants checkedPaths
  
  LevelDB.runDupes store $ do
    addAll $ catMaybes toAdd
    removeAll toRemove

checkPath :: FilePath -> IO CheckedPath
checkPath path = do
  exists <- doesFileExist path
  if exists
    then (liftM Canonical (canonicalizePath path)) `catch` errorMessage
    else return nonExistant
  where
    errorMessage :: IOException -> IO CheckedPath
    errorMessage ex = do
      putStrLn . ("error: " ++) $ show ex -- TODO Should be a logging statement
      return nonExistant
    nonExistant = NonExistant path

catCanonicals :: [CheckedPath] -> [FilePath]
catCanonicals ps = [p | Canonical p <- ps]

catNonExistants :: [CheckedPath] -> [FilePath]
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

removeAll :: (DupesMonad m) => [FilePath] -> DupesT m ()
removeAll = mapM_ remove

addAll :: (DupesMonad m) => [(FilePath, BucketKey)] -> DupesT m ()
addAll = mapM_ (\(path, key) -> add path key)
