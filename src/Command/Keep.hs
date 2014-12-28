{-# LANGUAGE Rank2Types #-}

module Command.Keep (
    Options
  , parserInfo
  , run
) where

import           Command.ParseUtil
import           Dupes
import           Store.LevelDB
import           Store.Repository      as Repo

import           Control.Monad         (unless)
import qualified Data.ByteString.Char8 as C
import           Data.List             (isPrefixOf)
import           Data.Machine          hiding (run)
import qualified Data.Set              as Set
import           Options.Applicative

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
  unless (Set.null matches && not (Set.null others))
    $ mapM_ yield $ Set.elems others

matchSpecs :: [PathSpec] -> FilePath -> Bool
matchSpecs specs path = all (`matchSpec` path) specs

matchSpec :: PathSpec -> FilePath -> Bool
matchSpec = isPrefixOf
