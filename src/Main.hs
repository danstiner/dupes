
import           Options.Applicative
import           System.Log.Logger

import qualified App                 ()
import qualified Command.Commands    as Commands
import qualified Logging
import qualified Settings
import           Telemetry

data Options = Options
  { optCommand :: Commands.Command }

optionParser :: Parser Options
optionParser = Options <$> Commands.parser

main :: IO ()
main = execParser opts >>= runWithOptions where
  parser = helper <*> optionParser
  desc = fullDesc
         <> progDesc "Collection of utilities for content-addressed storage inspired by git"
  opts = info parser desc

runWithOptions :: Options -> IO ()
runWithOptions options = do
  appDir <- Settings.getAndCreateAppDir

  Logging.register appDir ERROR
  Telemetry.register appDir

  Telemetry.record Boot

  Commands.run (optCommand options)

  Telemetry.record Exit
