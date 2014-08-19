{-# LANGUAGE Rank2Types #-}

module Command.Dupes.Add (
    Options
  , parserInfo
  , run
) where

import Dupes
import Settings

import Control.Monad.Trans ( lift )
import Data.Foldable ( Foldable, traverse_ )
import Data.List ( delete, (\\), sort )
import Data.Machine hiding ( run )
import Data.Serialize (encode,decode)
import Options.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Database.LevelDB.Higher as Level
import System.Directory
import System.FilePath ( (</>) )

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
    machine = pathSource opt ~> processPath

pathSource :: Options -> SourceT IO FilePath
pathSource opt =
  if (optStdin opt || elem stdinFilename opaths)
    then optPathSource `joinSources` stdinPathSource
    else optPathSource
  where
    opaths = optPaths opt
    optPathSource = source (delete stdinFilename opaths)
    stdinFilename = "-"
    stdinPathSource :: SourceT IO FilePath
    stdinPathSource = sourceT $ do
      c <- getContents
      return (lines c)

processPath :: ProcessT IO FilePath ()
processPath = repeatedly $ do
  path <- await
  appDir <- lift $ Settings.getAppDir
  items <- lift $ Level.runCreateLevelDB (appDir </> "leveldb") (C.pack "Dupes") $ Level.withSnapshot $ Level.scan (encode $ toPathKey path) Level.queryItems

  let merged = (traversePath path ~> toPathKeyP) `mergeOrderedStreams` (source items ~> levelToPathKey) in
    lift $ runT_ (merged ~> store)

store :: ProcessT IO (MergedOperation PathKey) ()
store = repeatedly $ do
  key <- await

  lift $ case key of
    LeftOnly a -> putStr "Add " >> putStrLn (show a) >> put a
    RightOnly a -> putStr "Del " >> putStrLn (show a) >> rm a
    Both _ -> return ()

  where
    put key = do
      appDir <- Settings.getAppDir
      Level.runCreateLevelDB (appDir </> "leveldb") (C.pack "Dupes") $ Level.put (encode key) (encode key)
    rm key = do
      appDir <- Settings.getAppDir
      Level.runCreateLevelDB (appDir </> "leveldb") (C.pack "Dupes") $ Level.delete (encode key)

levelToPathKey :: ProcessT IO (Level.Key, Level.Value) PathKey
levelToPathKey = repeatedly $ do
  (key, _) <- await
  case decode key of
    Left _ -> return ()
    Right k -> yield k

toPathKeyP :: ProcessT IO FilePath PathKey
toPathKeyP = repeatedly $ do
  path <- await
  yield $ toPathKey path

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

sourceT :: Monad m => Foldable f => m (f b) -> SourceT m b
sourceT mxs = construct $ do
  xs <- lift mxs
  traverse_ yield xs

joinSources :: Monad m => SourceT m a -> SourceT m a -> SourceT m a
joinSources a b = capX a w <~ b
  where
    w = repeatedly $ do
      z <- awaits Z
      let e = either id id z
      yield e
