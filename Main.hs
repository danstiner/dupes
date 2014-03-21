
import System.Environment

import qualified App
import qualified Logging
import qualified Plumbing
import qualified Store.Mem as Mem
import qualified Store.Flat as Flat
import qualified Store.LevelDB as LevelDB
import qualified Store.RemoteHttp as Remote
import qualified Telemetry
import System.Directory as Dir
import System.Log.Logger
import System.FilePath ( (</>) )
import System.Directory as Directory
import Options.Applicative

data CommandOptions = CommandOptions
  { verbose :: Bool
  , quiet :: Bool
  , path :: String }

sample :: Parser CommandOptions
sample = CommandOptions
  <$> switch
      ( long "verbose"
     <> help "Whether to be loud" )
  <*> switch
      ( long "quiet"
     <> help "Whether to be quiet" )
  <*> argument str (metavar "PATH")


main :: IO ()
main = execParser opts >>= runWithOptions
	where
		parser = (helper <*> sample)
		desc = ( fullDesc
			<> progDesc "Print a greeting for TARGET"
			<> header "hello - a test for optparse-applicative" )
		opts = info parser desc

runWithOptions :: CommandOptions -> IO ()
runWithOptions options = do
  home <- Dir.getHomeDirectory
  let appUserDir = home </> ".clod"
  Directory.createDirectoryIfMissing False appUserDir

  let logLevel = case ((quiet options), (verbose options)) of
  	(True, False) -> ERROR
  	(False, True) -> DEBUG
  	otherwise -> WARNING

  Logging.registerLogger appUserDir logLevel
  Telemetry.registerLogger appUserDir

  infoM App.logTag "Application launching"

  treeId <- Plumbing.writeTree (path options) $ Flat.createStore (appUserDir)
  Plumbing.setRef "master" treeId $ LevelDB.createStore (appUserDir </> "leveldb")
  return ()
