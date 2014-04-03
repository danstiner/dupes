module Command.HashObject (
	hashObjectCommand
) where

import qualified Data.ByteString as B
import Options.Applicative

import qualified Blob
import Options
import Plumbing


data ObjectType = Blob | Tree

instance Read ObjectType where

data CommandOptions = CommandOptions
  { write :: Bool
  , readfromStdin :: Bool
  , readPathsFromStdin :: Bool
  , path :: FilePath
  , noFilters :: Bool }

optionParser :: Parser CommandOptions
optionParser = CommandOptions
  <$> switch
      ( short 'w'
     <> help "Actually write the object into the object database." )
  <*> switch
      ( long "stdin"
     <> help "Read the object from standard input instead of from a file." )
  <*> switch
      ( long "stdin-paths"
     <> help "Read file names from stdin instead of from the command-line." )
  <*> argument str (metavar "PATH")
  <*> switch
      ( long "no-filters"
     <> help "Hash the contents as is, ignoring any input filter that would have been chosen by the attributes mechanism, including the end-of-line conversion. If the file is read from standard input then this is always implied, unless the --path option is given." )

hashObjectCommand :: [String] -> IO ()
hashObjectCommand args =
	execParserWithArgs opts (take 1 args) >>= hashObjectWithOptions
	where
		parser = (helper <*> optionParser)
		desc = ( fullDesc
			<> progDesc "Compute object ID and optionally creates a blob from a file" )
		opts = info parser desc

hashObjectWithOptions :: CommandOptions -> IO ()
hashObjectWithOptions options = do
	file <- B.readFile (path options)

	let blobId = hashObject file

	putStrLn (Blob.toString blobId)

	return ()