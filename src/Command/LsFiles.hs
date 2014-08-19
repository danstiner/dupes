module Command.LsFiles (
    Options
  , parserInfo
  , run
) where

import Data.Binary as Binary
import Data.ByteString
import Options.Applicative
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Database.LevelDB.Higher as Level
import System.FilePath ( (</>) )

import qualified Blob
import qualified Settings

data Options = Options
  { _cached :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the index and the working tree")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "cached"
     <> short 'c'
     <> help "Show cached files in the output (default)." )

keySpace :: ByteString
keySpace = C.pack "MyKeySpace"

run :: Options -> IO ()
run _ = do
  appDir <- Settings.getAppDir
  let path = appDir </> "leveldb"
  
  items <- Level.runLevelDB path Level.def (Level.def, Level.def) keySpace $ do
    Level.scan (C.pack ("index/")) (Level.queryItems)

  mapM_ (Prelude.putStrLn . showItem) items

  return ()


showItem :: Level.Item -> String
showItem item =
  (C.unpack $ fst item)
  ++ " " ++
  (Blob.toHexString $ Binary.decode $ L.fromStrict $ snd item)
