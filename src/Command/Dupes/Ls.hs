module Command.Dupes.Ls (
    Options
  , parserInfo
  , run
) where

import Dupes
import Store.LevelDB

import Options.Applicative
import qualified Data.Set as Set

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
run (Options {_optShowDupesOnly=False}) = runStoreOp bucketsOp >>= mapM_ printBucket
run (Options {_optShowDupesOnly=True}) = runStoreOp bucketsOp >>= mapM_ printBucket . filterDupes
  where
    filterDupes = filter (\(Bucket _ paths) -> 1 < Set.size paths)

printBucket :: Bucket -> IO ()
printBucket = putStrLn . showBucket
  where
    showBucket (Bucket key paths) = show key ++ " " ++ showPaths paths
    showPaths = Set.fold (\a b -> a ++ " " ++ b) "" . Set.map unPathKey
