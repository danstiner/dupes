
import qualified App
import qualified Logging
import qualified Telemetry
import System.Directory as Dir
import System.Log.Logger
import System.FilePath ( (</>) )
import System.Directory as Directory
import Options.Applicative

import qualified Command.Commands as Commands

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
  home <- Dir.getHomeDirectory
  let appUserDir = home </> ".clod"
  Directory.createDirectoryIfMissing False appUserDir

  --let logLevel = case ((quiet options), (verbose options)) of
  --	(True, False) -> ERROR
  --	(False, True) -> DEBUG
  --	(_, _) -> WARNING

  --Logging.registerLogger appUserDir logLevel
  Telemetry.registerLogger appUserDir

  infoM App.logTag "Application launching"

  Commands.run (optCommand options)

  return ()
