module Command.Init (
    Options
  , parserInfo
  , run
) where

import           Options.Applicative
import           Repository
import           System.Directory

data Options = Options
  { _optQuiet :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Create a repository")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "quiet"
     <> short 'q'
     <> help "Only print warning and error messages.")

run :: Options -> IO ()
run _ = getCurrentDirectory >>= Repository.create
