module Command.Dupes.Ls (
    Options
  , parserInfo
  , run
) where

import Dupes
import Store.LevelDB
import Store.Repository as Repo
import Telemetry as Telemetry

import Control.Monad ( foldM )
import Options.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as Set
import System.TimeIt as TimeIt

data Options = Options
  { _optAll :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the duplicate index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "all"
     <> help "Show not just duplicate files." )

run :: Options -> IO ()
run (Options {_optAll=True}) = printListing bucketsOp
run (Options {_optAll=False}) = printListing dupesOp

printListing :: StoreOp [Bucket] -> IO ()
printListing op = recordTelemetry $ do
  repo <- Repo.get
  buckets <- runStoreOp (getStore repo) op
  foldM printAndInc 0 buckets
  where
    printAndInc n bucket = printBucket bucket >> (return $! n + 1)
    recordTelemetry m = do
      (time, count) <- TimeIt.timeItT m
      Telemetry.record (DupesLs time count)

printBucket :: Bucket -> IO ()
printBucket = putStrLn . showBucket
  where
    showBucket (Bucket key paths) = show key ++ " " ++ showPaths paths
    showPaths = Set.fold (\a b -> a ++ " " ++ b) "" . Set.map (C.unpack . unPathKey)
