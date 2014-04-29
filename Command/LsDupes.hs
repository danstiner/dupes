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
import Store.LevelDB as LevelDB

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

run :: Options -> IO ()
run opt = do

  appDir <- Settings.getAppDir

  let store = LevelDB.createStore (appDir </> "leveldb")
  files <- LevelDB.runLevelDBDupes (ls opt) store
  mapM Prelude.putStrLn files

  return ()

ls :: (DupesMonad m) => Options -> Dupes m [FilePath]
ls opt = do
    buckets <- list CRC32
    let bs = List.filter f buckets
    let nestedEntries = List.map (getEntries) bs
    let entries = List.concat nestedEntries
    return $ List.map getPaths entries

  where
    f = if (optDupeOnly opt) then isDupe else tru
    tru _ = True
    isDupe bucket = (1 < (List.length $ getEntries bucket) )
    getEntries (Bucket _ e) = e
    getPaths (Entry p) = p
