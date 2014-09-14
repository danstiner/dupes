module Command.Dupes.Ls (
    Options
  , parserInfo
  , run
) where

import Dupes
import Store.LevelDB
import Store.Repository as Repo
import Telemetry as Telemetry

import Control.Monad (foldM)
import Data.Word
import Options.Applicative
import qualified Data.Set as Set
import System.TimeIt as TimeIt

data Options = Options
  { _optShowDupesOnly :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the duplicate index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "dupe-only"
     <> help "Show only duplicate files." )

run :: Options -> IO ()
run (Options {_optShowDupesOnly=False}) = listDupes $ \_ -> True
run (Options {_optShowDupesOnly=True }) = listDupes $ \(Bucket _ paths) -> (1 < Set.size paths)

listDupes :: (Bucket -> Bool) -> IO ()
listDupes f = recordTelemetry $ do
  repo <- Repo.get
  buckets <- runStoreOp (getStore repo) bucketsOp

  foldM printAndInc 0 $ filter f buckets

  where
    printAndInc counter bucket = printBucket bucket >> return (counter + 1)

recordTelemetry :: IO Word64 -> IO ()
recordTelemetry m = do
  (time, count) <- TimeIt.timeItT m
  Telemetry.record (DupesLs time count)

printBucket :: Bucket -> IO ()
printBucket = putStrLn . showBucket
  where
    showBucket (Bucket key paths) = show key ++ " " ++ showPaths paths
    showPaths = Set.fold (\a b -> a ++ " " ++ b) "" . Set.map unPathKey
