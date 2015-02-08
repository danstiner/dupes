module Command.Init (
    Options
  , parserInfo
  , run
) where

import           Options.Applicative
import           Repository
import           System.Directory
import           System.Log.Logger

logTag :: String
logTag = "Command.Init"

data Options = Options
  { _optQuiet :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Create repository in the current directory if one does not already exist")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "quiet"
     <> short 'q'
     <> help "Only print warning and error messages.")

run :: Options -> IO ()
run _ = getCurrentDirectory >>= createRepository

createRepository :: FilePath -> IO ()
createRepository path = do
  Repository.create path
  infoM logTag ("Initialized empty repository at " ++ path)
