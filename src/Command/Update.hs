module Command.Update (
      Options
    , parserInfo
    , run
    ) where

import           Repository

import           Control.Monad.Trans.Resource
import           Options.Applicative

data Options = Options
  { _optQuiet :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Update entries in the duplicate file index to match the filesystem.")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "quiet"
     <> short 'q'
     <> help "Only print warning and error messages.")

run :: Options -> IO ()
run _ = get >>= \r -> runResourceT (withRepository r update)
