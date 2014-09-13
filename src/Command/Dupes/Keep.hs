{-# LANGUAGE Rank2Types #-}

module Command.Dupes.Keep (
    Options
  , parserInfo
  , run
) where

import Command.ParseUtil
import Dupes
import Store.LevelDB

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Trans ( lift )
import Data.List ( delete, (\\), sort, isPrefixOf )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Machine hiding ( run )
import Data.Machine.Fanout
import Data.Machine.Interleave
import Options.Applicative
import qualified Data.ByteString.Lazy as L
import System.Directory
import System.FilePath ( (</>) )
import System.IO
import Debug.Trace

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
  pathspecs <- runT $ pathspecSource (optPaths opt) (optStdin opt)
  buckets <- runStoreOp bucketsOp
  runT_ $ source buckets ~> filterBuckets pathspecs ~> autoM putStrLn

type PathSpec = FilePath

filterBuckets :: [PathSpec] -> Process Bucket FilePath
filterBuckets specs = repeatedly $ do
  (Bucket _ pathSet) <- await
  let (matches, others) = Set.partition (matchSpecs specs) $ Set.map unPathKey pathSet
  if (Set.null matches && not (Set.null others))
    then return ()
    else mapM_ yield $ Set.elems others

matchSpecs :: [PathSpec] -> FilePath -> Bool
matchSpecs specs path = and $ map (\s -> matchSpec s path) specs

matchSpec :: PathSpec -> FilePath -> Bool
matchSpec spec path = isPrefixOf spec path
