
import Options.Applicative
import System.Directory as Directory
import System.Log.Logger

import qualified App ()
import qualified Command.Commands as Commands
import qualified Logging
import qualified Settings
import qualified Telemetry

data Options = Options
  { optCommand :: Commands.Command }

optionParser :: Parser Options
optionParser = Options
  <$> Commands.parser

main :: IO ()
main = do
  execParser opts >>= runWithOptions
	where
		parser = (helper <*> optionParser)
		desc = ( fullDesc
			<> progDesc "Print a greeting for TARGET"
			<> header "hello - a test for optparse-applicative" )
		opts = info parser desc

runWithOptions :: Options -> IO ()
runWithOptions options = do
  appUserDir <- Settings.getAppDir
  Directory.createDirectoryIfMissing False appUserDir

  Logging.register appUserDir ERROR
  Telemetry.register appUserDir

  Telemetry.recordLaunch

  Commands.run (optCommand options)

  Telemetry.recordExit
