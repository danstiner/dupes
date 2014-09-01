module Command.Dupes.Ls (
    Options
  , parserInfo
  , run
) where

import Dupes
import Store.LevelDB

import Options.Applicative

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
run _ = runStoreOp listAll >>= mapM_ putStrLn . map show
  where
    listAll = listOp root
    root = toPathKey ""
