{-# LANGUAGE Rank2Types #-}

module Command.Dupes.Add (
    Options
  , parserInfo
  , run
) where

import Dupes

import Control.Monad.Trans ( lift )
import Data.Foldable ( Foldable, traverse_ )
import Data.List ( delete, (\\), sort )
import Data.Machine hiding ( run )
import Options.Applicative
import System.FilePath ( (</>) )
import System.Directory

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
    machine = pathSource opt ~> traversePath ~> toPathKeyP ~> printPathKey

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

toPathKeyP :: ProcessT IO FilePath PathKey
toPathKeyP = repeatedly $ do
  path <- await
  yield $ toPathKey path

traversePath :: ProcessT IO FilePath FilePath
traversePath = repeatedly $ do
  path <- await
  traversePath' path

traversePath' path = do
  yield path
  isDir <- lift $ doesDirectoryExist path
  if isDir
    then do
      contents <- lift $ getDirectoryContents path
      let actual = sort . map (path </>) $ contents \\ [".", ".."]
      mapM_ traversePath' actual
    else return ()

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
