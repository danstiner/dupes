module Command.Init (Options, parserInfo, run) where

import           Control.Monad       (void)
import           Options.Applicative
import           Dupes.Repository as Repository

data Options = Options { _optQuiet :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser (progDesc "Create a repository")

parser :: Parser Options
parser = Options
         <$> switch
               (long "quiet"
                <> short 'q'
                <> help "Only print warning and error messages.")

run :: Options -> IO ()
run _ = void Repository.create
