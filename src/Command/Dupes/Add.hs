{-# LANGUAGE Rank2Types #-}

module Command.Dupes.Add (
    Options
  , parserInfo
  , run
) where

import Dupes
import Store.LevelDB

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans ( lift )
import Data.List ( delete, (\\), sort )
import Data.Machine hiding ( run )
import Data.Machine.Interleave
import Options.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory
import System.FilePath ( (</>) )
import System.IO

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
run opt = runT_ machine
  where
    machine = pathSource opt ~> processPaths

pathSource :: Options -> SourceT IO FilePath
pathSource opt =
  if (optStdin opt || elem stdinFilename (optPaths opt))
    then stdinLines ~> prepended actualOptPaths
    else source actualOptPaths
  where
    actualOptPaths = delete stdinFilename (optPaths opt)
    stdinFilename = "-"
    stdinLines :: SourceT IO FilePath
    stdinLines = construct $ stdinLinesPlan
    stdinLinesPlan = do
      eof <- lift isEOF
      unless eof $ do
        line <- lift getLine
        yield line
        stdinLinesPlan

processPaths :: ProcessT IO FilePath ()
processPaths = repeatedly $ await >>= lift . processPath

processPath :: FilePath -> IO ()
processPath path = runT_ $ runDBActions $ (traverse ~> mergePaths ~> prep ~> store)
  where
    runDBActions = fitM runDupesDBT
    prep = fitM lift prepMergeOp
    traverse = fitM lift (traversePath path ~> toPathKeyP)
    mergePaths = fitM storeOpToDBAction (mergeProcess path)
    store = fitM storeOpToDBAction (storeFree)

prepMergeOp :: ProcessT IO (MergedOperation PathKey) (MergedOperation (PathKey, BucketKey))
prepMergeOp = repeatedly $ do
  mergeOp <- await
  case mergeOp of
    Both pathKey -> yield $ Both (pathKey, undefined)
    RightOnly p -> yield $ RightOnly (p, undefined)
    LeftOnly p -> do
      mBucketKey <- lift $ calcBucketKey $ unPathKey p
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

traversePath :: FilePath -> SourceT IO FilePath
traversePath = construct . plan
  where
  plan path = do
    isDir <- lift $ doesDirectoryExist path

    yield path

    if isDir
      then do
        contents <- lift $ getDirectoryContents path
        let actual = map (path </>) $ contents \\ [".", ".."]
        slashed <- lift $ addSlashes actual
        mapM_ plan $ sort slashed
      else return ()
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
      let key = createBucketKeyLazy CRC32 c
      key `deepseq` (return $! Just key)
    errorMessage :: IOException -> IO (Maybe BucketKey)
    errorMessage ex = do
      putStrLn . ("error: " ++) $ show ex
      return Nothing
