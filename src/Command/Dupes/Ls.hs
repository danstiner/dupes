module Command.Dupes.Ls (
    Options
  , parserInfo
  , run
) where

import Dupes
import Store.LevelDB
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
run (Options {_optShowDupesOnly=False}) = recordTelemetry $ runStoreOp bucketsOp >>= foldM printAndInc 0
run (Options {_optShowDupesOnly=True }) = recordTelemetry $ runStoreOp bucketsOp >>= return . filterDupes >>= foldM printAndInc 0
  where
    filterDupes = filter (\(Bucket _ paths) -> 1 < Set.size paths)

printAndInc :: Num n => n -> Bucket -> IO n
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
