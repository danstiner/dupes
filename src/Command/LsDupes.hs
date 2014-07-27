module Command.LsDupes (
	Options
  , parserInfo
  , run
) where

import Data.List as List
import Options.Applicative
import System.FilePath ( (</>) )

import Dupes
import qualified Settings
import qualified Telemetry
import Store.LevelDB as LevelDB

data Options = Options
  { optShowDupesOnly :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the duplicate index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "dupe-only"
     <> help "Show only duplicate files." )

run :: Options -> IO ()
run opt = do

  appDir <- Settings.getAppDir

  let store = LevelDB.createStore (appDir </> "leveldb")
  fileSets <- LevelDB.runDupes store (ls opt)
  let fileLines = map (foldr1 combine) fileSets
  mapM_ Prelude.putStrLn fileLines

  Telemetry.recordLsDupes (length fileSets)

  where
    combine l r = l ++ "," ++ r

ls :: (DupesMonad m) => Options -> DupesT m [[FilePath]]
ls opt = do
    buckets <- buckets CRC32
    let bs = List.filter bucketFilter buckets
    let nestedEntries = List.map (getEntries) bs
    return $ List.map (List.map getPaths) nestedEntries

  where
    bucketFilter = if (optShowDupesOnly opt) then isDupe else tru
    tru _ = True
    isDupe bucket = (1 < (List.length $ getEntries bucket) )
    getEntries (Bucket _ e) = e
    getPaths (Entry p) = p
