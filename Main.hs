
import qualified App
import qualified Logging
import qualified Plumbing
import qualified Store.Flat as Flat
import qualified Store.LevelDB as LevelDB
import qualified Telemetry
import System.Directory as Dir
import System.Log.Logger
import System.FilePath ( (</>) )
import System.Directory as Directory
import System.Environment (getArgs)
import Options.Applicative

import Options
import qualified Command.Commands as Commands

data CommandOptions = CommandOptions
  { commandName :: String }

optionParser :: Parser CommandOptions
optionParser = CommandOptions
  <$> argument str (metavar "COMMAND")

main :: IO ()
main = do
  args <- getArgs
  execParserWithArgs opts (take 1 args) >>= runWithOptions
	where
		parser = (helper <*> optionParser)
		desc = ( fullDesc
			<> progDesc "Print a greeting for TARGET"
			<> header "hello - a test for optparse-applicative" )
		opts = info parser desc

runWithOptions :: CommandOptions -> IO ()
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

  args <- getArgs

  Commands.run (commandName options) (drop 1 args)

  --treeId <- Plumbing.writeTree (path options) $ Flat.createStore (appUserDir)
  --Plumbing.setRef "master" treeId $ LevelDB.createStore (appUserDir </> "leveldb")
  return ()
