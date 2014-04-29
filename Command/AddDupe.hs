module Command.AddDupe (
	Options
  , parserInfo
  , run
) where

import Control.Exception
import Control.Monad
import System.IO
import Data.Maybe ( catMaybes )
import Options.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory ( canonicalizePath, doesFileExist )
import System.FilePath ( (</>) )

import Dupes
import qualified Settings
import Store.LevelDB as LevelDB

data Options = Options
  { optStdin :: Bool
  , optPaths :: [FilePath]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the index and the working tree")

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

  paths <- getPaths
  cPaths <- mapM canonicalize paths
  let justPaths = catMaybes cPaths
  toAdd <- filterM shouldAdd justPaths
  pairs <- mapM keyPair toAdd
  let justPairs = catMaybes pairs

  LevelDB.runLevelDBDupes (addAll justPairs) store
  where
    getPaths = if (optStdin opt)
      then readPaths
      else return (optPaths opt)
    readPaths = liftM lines getContents

canonicalize :: FilePath -> IO (Maybe FilePath)
canonicalize p = (liftM Just (canonicalizePath p)) `catch` errorMessage
  where
    errorMessage :: IOException -> IO (Maybe FilePath)
    errorMessage ex = do
      putStrLn . ("error: " ++) $ show ex
      return Nothing

keyPair :: FilePath -> IO (Maybe (FilePath, BucketKey))
keyPair path = do
  bucketKey <- calcBucketKey path
  case bucketKey of
    Nothing -> return Nothing
    Just key -> return $ Just (path, key)

calcBucketKey :: FilePath -> IO (Maybe BucketKey)
calcBucketKey path = calc `catch` errorMessage
  where
    calc = withFile path ReadMode $ \hnd -> do
      c <- (L.hGetContents hnd)
      key <- evaluate $ createBucketKey MD5 c
      return $ Just key
    errorMessage :: IOException -> IO (Maybe BucketKey)
    errorMessage ex = do
      putStrLn . ("error: " ++) $ show ex
      return Nothing

shouldAdd :: FilePath -> IO Bool
shouldAdd = doesFileExist

addAll :: (DupesMonad m) => [(FilePath, BucketKey)] -> Dupes m ()
addAll = mapM_ (\(path, key) -> addDupe path key)

addDupe :: (DupesMonad m) => FilePath -> BucketKey -> Dupes m ()
addDupe = add


