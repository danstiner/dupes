{-# LANGUAGE Rank2Types #-}

module Command.Dupes.Keep (
    Options
  , parserInfo
  , run
) where

import Command.ParseUtil
import Dupes
import Store.LevelDB
import Store.Repository as Repo

import Data.List ( isPrefixOf )
import Data.Machine hiding ( run )
import Options.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as Set

data Options = Options
  { optStdin :: Bool
  , optPaths :: [FilePath]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Discard all duplicates that have at least one copy in the specified pathspec's")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "stdin"
     <> help "Read pathspec's from STDIN" )
  <*> many
      ( argument str (metavar "PATHSPEC") )

run :: Options -> IO ()
run opt = do
  repo <- Repo.get
  pathspecs <- runT $ pathspecSource (optPaths opt) (optStdin opt)
  buckets <- runStoreOp (getStore repo) bucketsOp
  runT_ $ source buckets ~> filterBuckets pathspecs ~> autoM putStrLn

filterBuckets :: [PathSpec] -> Process Bucket FilePath
filterBuckets specs = repeatedly $ do
  (Bucket _ pathSet) <- await
  let (matches, others) = Set.partition (matchSpecs specs) $ Set.map (C.unpack . unPathKey) pathSet
  if (Set.null matches && not (Set.null others))
    then return ()
    else mapM_ yield $ Set.elems others

matchSpecs :: [PathSpec] -> FilePath -> Bool
matchSpecs specs path = and $ map (\s -> matchSpec s path) specs

matchSpec :: PathSpec -> FilePath -> Bool
matchSpec spec path = isPrefixOf spec path
