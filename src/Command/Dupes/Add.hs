{-# LANGUAGE Rank2Types #-}

module Command.Dupes.Add (
    Options
  , parserInfo
  , run
) where

import Dupes

import Control.Monad.Trans ( lift )
import Data.Foldable ( Foldable, traverse_ )
import Data.List ( delete )
import Data.Machine hiding ( run )
import Options.Applicative
import System.Directory.Tree

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
     machine = pathSource opt ~> toDirTree ~> traverseTree ~> printPathKey

printPathKey :: ProcessT IO PathKey ()
printPathKey = repeatedly $ await >>= \a -> lift (putStrLn $ show a)

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

toDirTree :: ProcessT IO FilePath (AnchoredDirTree PathKey)
toDirTree = repeatedly $ do
  path <- await
  dir <- lift $ readDirectoryWithL (return . toPathKey) path
  yield dir

traverseTree :: ProcessT IO (AnchoredDirTree PathKey) PathKey
traverseTree = repeatedly $ do
  atree <- await
  traverse_ yield (dirTree atree)

sourceT :: Monad m => Foldable f => m (f b) -> SourceT m b
sourceT mxs = construct $ do
  xs <- lift mxs
  traverse_ yield xs

joinSources :: Monad m => SourceT m a -> SourceT m a -> SourceT m a
joinSources a b = (capX a w)  <~  b
  where
    w = repeatedly $ do
      z <- awaits Z
      let e = either id id z
      yield e
