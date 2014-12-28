module Command.Init (
    Options
  , parserInfo
  , run
) where

import qualified App
import           Options.Applicative
import           Store.Repository    as R
import           System.Directory
import           System.Log.Logger

logTag :: String
logTag = App.logTag ++ ".Command.Init"

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
run _ = do
    path <- R.getPath
    createDirectory path
    infoM logTag ("Initialized empty repository at " ++ path)
