module Command.AddDupe (
	Options
  , parserInfo
  , run
) where

import Control.Monad.Trans.Class (lift)
import Data.ByteString
import Data.List as List
import Options.Applicative
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Database.LevelDB.Higher as Level
import System.Directory (doesFileExist)
import System.FilePath ( (</>) )

import qualified Blob
import qualified Settings

data Options = Options
  { optPaths :: [FilePath]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the index and the working tree")

parser :: Parser Options
parser = Options
  <$> many
      ( argument str (metavar "PATH") )

keySpace :: ByteString
keySpace = C.pack "Dupes"

run :: Options -> IO ()
run opt = do
  appDir <- Settings.getAppDir
  let path = appDir </> "leveldb"
  
  items <- Level.runLevelDB path Level.def (Level.def, Level.def) keySpace $
    mapM put (optPaths opt)

  return ()

put :: FilePath -> Level.LevelDBT IO ()
put path = do
  onDisk <- lift $ doesFileExist path
  if onDisk
   then do
    bytes <- lift $ B.readFile path
    let key = hash bytes
    prev <- Level.get key
    lift $ Prelude.putStrLn path
    case prev of
      Just v -> Level.put key (L.toStrict $ Binary.encode $ List.nub $ (path : (Binary.decode $ L.fromStrict v :: [FilePath]) ))
      Nothing -> Level.put key (L.toStrict $ Binary.encode $ [path])
   else return ()
  where
    hash bytes = L.toStrict $ Binary.encode $ Blob.createId $ Blob.Bytes bytes
