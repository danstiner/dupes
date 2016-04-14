module Command.Update (Options, parserInfo, run) where

import qualified Dupes.Repository as Repository
import qualified Store

import           Options.Applicative

data Options = Options { _optQuiet :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser (progDesc "Update index to match working directory")

parser :: Parser Options
parser = Options
         <$> switch
               (long "quiet"
                <> short 'q'
                <> help "Only print warning and error messages.")

run :: Options -> IO ()
run _ = Repository.find >>= Store.update >>= putStr . show
