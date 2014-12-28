{-# LANGUAGE Rank2Types #-}

module Command.Add (
    Options
  , parserInfo
  , run
) where

import Command.ParseUtil
import Dupes
import qualified App
import Store.LevelDB
import Store.Repository as Repo

import Control.Monad (when)
import Control.DeepSeq
import Control.Exception
import Control.Monad.Trans ( lift )
import Data.List ( (\\), sort )
import Data.Machine hiding ( run )
import Data.Machine.Interleave
import Options.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import System.Directory
import System.FilePath ( (</>) )
import System.IO
import System.Log.Logger

logTag :: String
logTag = App.logTag ++ ".Command.Add"

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
run opt = runT_ . machine =<< Repo.get
  where
    machine repo = pathspecs ~> processPaths (Repo.getStore repo)
    pathspecs = pathspecSource (optPaths opt) (optStdin opt)

processPaths :: Store -> ProcessT IO PathSpec ()
processPaths store = repeatedly $ await >>= lift . processPath store

processPath :: Store -> PathSpec -> IO ()
processPath s path = runT_ . runDBActions $ (traverse ~> mergeExisting ~> hashNew ~> store)
  where
    runDBActions = fitM (runDupesDBT s)
    hashNew = fitM lift hash
    traverse = fitM lift (traversePathSpec path ~> toPathKeyP)
    mergeExisting = fitM storeOpToDBAction (mergeProcess path)
    store = fitM storeOpToDBAction storeFree

hash :: ProcessT IO (MergedOperation PathKey) (MergedOperation (PathKey, BucketKey))
hash = repeatedly $ do
  mergeOp <- await
  case mergeOp of
    Both pathKey -> yield $ Both (pathKey, undefined)
    RightOnly p -> yield $ RightOnly (p, undefined)
    LeftOnly p -> do
      mBucketKey <- lift . calcBucketKey . C.unpack $ unPathKey p
      case mBucketKey of
        Just bucketKey -> yield $ LeftOnly (p, bucketKey)
        Nothing -> return ()

mergeProcess :: FilePath -> ProcessT StoreOp PathKey (MergedOperation PathKey)
mergeProcess path = cappedMerge (listChildren path)

listChildren :: FilePath -> SourceT StoreOp PathKey
listChildren path = construct $ (lift . listOp . toPathKey) path >>= mapM_ yield

cappedMerge :: (Ord a) => SourceT StoreOp a -> ProcessT StoreOp a (MergedOperation a)
cappedMerge src = capYM src mergeOrderedStreamsWye

storeFree :: ProcessT StoreOp (MergedOperation (PathKey, BucketKey)) ()
storeFree = repeatedly $ do
  v <- await
  lift $ case v of
    LeftOnly  (p, b) -> putOp p b
    RightOnly (p, _) -> rmOp p
    Both _ -> return ()

traversePathSpec :: FilePath -> SourceT IO FilePath
traversePathSpec = construct . plan
  where
  plan path = do
    isDir <- lift $ doesDirectoryExist path
    yield path
    when isDir $
      do  contents <- lift $ getDirectoryContents path
          let actual = map (path </>) $ contents \\ [".", ".."]
          slashed <- lift $ addSlashes actual
          mapM_ plan $ sort slashed
  addSlashes = mapM addSlash
  addSlash path = do
    isDir <- doesDirectoryExist path
    if isDir
      then return (path ++ "/")
      else return path

toPathKeyP :: ProcessT IO FilePath PathKey
toPathKeyP = repeatedly $ await >>= yield . toPathKey

calcBucketKey :: FilePath -> IO (Maybe BucketKey)
calcBucketKey path = calc `catch` errorMessage
  where
    calc = withBinaryFile path ReadMode $ \hnd -> do
      c <- L.hGetContents hnd
      let key = createBucketKey CRC32 c
      key `deepseq` return (Just key)
    errorMessage :: IOException -> IO (Maybe BucketKey)
    errorMessage ex = do
      warningM logTag (show ex)
      return Nothing
