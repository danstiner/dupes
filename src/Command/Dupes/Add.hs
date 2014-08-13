{-# LANGUAGE Rank2Types #-}

module Command.Dupes.Add (
    Options
  , parserInfo
  , run
) where

import Control.Monad.Trans ( lift )
import Data.Foldable ( Foldable, traverse_ )
import Data.List ( delete )
import Data.Machine hiding ( run )
import Options.Applicative

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
     machine = pathSource opt ~> printPath

printPath :: ProcessT IO FilePath ()
printPath = repeatedly $ await >>= \a -> lift (putStrLn a)

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
