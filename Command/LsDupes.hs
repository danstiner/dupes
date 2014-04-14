module Command.LsDupes (
	Options
  , parserInfo
  , run
) where

import Data.Binary as Binary
import Data.ByteString
import Data.List as List
import Data.String.Utils as StrUtils
import Options.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Database.LevelDB.Higher as Level
import System.FilePath ( (</>) )

import qualified Blob
import qualified Settings

data Options = Options
  { optDupeOnly :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the index and the working tree")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "dupe-only"
     <> help "Show only duplicate files." )

keySpace :: ByteString
keySpace = C.pack "Dupes"

run :: Options -> IO ()
run opt = do

  appDir <- Settings.getAppDir
  let path = appDir </> "leveldb"
  let f = if (optDupeOnly opt) then dupe else tru
  
  items <- Level.runLevelDB path Level.def (Level.def, Level.def) keySpace $ do
    Level.scan (C.pack "") Level.queryItems

  mapM (\i -> Prelude.putStrLn $ showItem i) (List.filter f items)

  return ()
  where
    tru _ = True
    dupe item = (1 < (List.length $ decodeValue $ snd item) )

decodeValue :: B.ByteString -> [FilePath]
decodeValue b = Binary.decode $ L.fromStrict b

showItem :: Level.Item -> String
showItem item =
  (Blob.toString $ Binary.decode $ L.fromStrict $ fst item)
  ++ " " ++
  (StrUtils.join ":" (decodeValue $ snd item))
