module Command.LsDupes (
    Options
  , parserInfo
  , run
) where

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
run _ = undefined
