module Command.UpdateIndex (
    Options
  , parserInfo
  , run
) where

import qualified Data.ByteString as B
import Options.Applicative
import System.Directory (doesFileExist)
import System.FilePath ( (</>) )

import qualified Settings
import Store.LevelDB as LevelDB
import Index
import qualified Blob
import Text.Read (step, readPrec)

data CacheInfoParams = NoCacheInfoParams | CacheInfoParams Int Blob.Id FilePath

instance Read CacheInfoParams where
  readPrec = do
    m <- step readPrec
    i <- step readPrec
    path <- step readPrec
    return $ CacheInfoParams m i path


data Options = Options
  { optAdd :: Bool
  --, optRemove :: Bool
  --, optRefresh :: Bool
  --, optForceRemove :: Bool
  --, optReplace :: Bool
  --, optQuiet :: Bool
  --, optUnmerged :: Bool
  --, optIgnoreMissing :: Bool
  --, optAssumeUnchanged :: Bool
  --, optSkipWorkTree :: Bool
  --, optReallyRefresh :: Bool
  , optCacheInfo :: CacheInfoParams
  --, optVerbose :: Bool
  , optPath :: [FilePath] }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Register file contents in the working tree to the index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "add"
     <> help "If a specified file isn't in the index already then it's added. Default behaviour is to ignore new files." )
  -- <*> switch
  --    ( long "remove"
  --   <> help "If a specified file is in the index but is missing then it's removed. Default behavior is to ignore removed file." )
  -- <*> switch
  --    ( long "refresh"
  --   <> help "Looks at the current index and checks to see if merges or updates are needed by checking stat() information." )
  <*> option
      ( long "cacheinfo"
     <> value NoCacheInfoParams
     <> metavar "<mode> <object> <path>"
     <> help "Looks at the current index and checks to see if merges or updates are needed by checking stat() information." )
  <*> many
      ( argument str (metavar "PATH"))

run :: Options -> IO ()
run opt = do
  runCacheInfo (optCacheInfo opt)
  runUpdateInfos opt (optPath opt)

runUpdateInfos :: Options -> [FilePath] -> IO ()
runUpdateInfos opt paths = do
  mapM_ (runSingleUpdateInfo opt) paths

runSingleUpdateInfo :: Options -> FilePath -> IO ()
runSingleUpdateInfo opt path = do
  onDisk <- doesFileExist path
  let inIndex = False

  if onDisk
    then if inIndex
      then updateInfo path
      else if (optAdd opt)
        then updateInfo path
        else return ()
    else if inIndex
      then return () -- TODO removeInfo path
      else return ()

  return ()

runCacheInfo :: CacheInfoParams -> IO ()
runCacheInfo NoCacheInfoParams = do return ()
runCacheInfo (CacheInfoParams _ hash path) = do
  appDir <- Settings.getAppDir
  let store = LevelDB.createStore (appDir </> "leveldb")
  LevelDB.runLevelDBIndex (set path hash) store
  return ()

updateInfo :: FilePath -> IO ()
updateInfo path = do
  putStrLn $ "Update info for " ++ path
  exist <- doesFileExist path
  if exist
    then do
      appDir <- Settings.getAppDir
      bytes <- B.readFile path
      let store = LevelDB.createStore (appDir </> "leveldb")
      LevelDB.runLevelDBIndex (updateHash path bytes) store
      return ()
    else return ()

updateHash :: (IndexMonad m) => FilePath -> B.ByteString -> Index m ()
updateHash path contents = do
    set path hash
  where
    hash = Blob.createId $ Blob.Bytes contents
