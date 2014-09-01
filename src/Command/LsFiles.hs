module Command.LsFiles (
    Options
  , parserInfo
  , run
) where

import Options.Applicative

data Options = Options
  { _cached :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the index and the working tree")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "cached"
     <> short 'c'
     <> help "Show cached files in the output (default)." )

run :: Options -> IO ()
run _ = undefined
