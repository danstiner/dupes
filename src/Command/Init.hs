module Command.Init (
    Options
  , parserInfo
  , run
) where

import           Options.Applicative
import           Repository
import Control.Monad (void)

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
run _ = void Repository.create
