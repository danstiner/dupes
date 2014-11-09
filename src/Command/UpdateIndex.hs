module Command.UpdateIndex (
    Options
  , parserInfo
  , run
) where

import Options.Applicative
import Text.Read (step, readPrec)

import qualified Blob

data CacheInfoParams = NoCacheInfoParams | CacheInfoParams Int Blob.Id FilePath

--instance Read CacheInfoParams where
--  readPrec = do
--    m <- step readPrec
--    i <- step readPrec
--    path <- step readPrec
--    return $ CacheInfoParams m i path


data Options = Options
  { _optAdd :: Bool
  , _optRemove :: Bool
  , _optRefresh :: Bool
  --, optForceRemove :: Bool
  --, optReplace :: Bool
  --, optQuiet :: Bool
  --, optUnmerged :: Bool
  --, optIgnoreMissing :: Bool
  --, optAssumeUnchanged :: Bool
  --, optSkipWorkTree :: Bool
  --, optReallyRefresh :: Bool
  --, _optCacheInfo :: CacheInfoParams
  --, optVerbose :: Bool
  , _optPath :: [FilePath] }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Register file contents in the working tree to the index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "add"
     <> help "If a specified file isn't in the index already then it's added. Default behaviour is to ignore new files." )
   <*> switch
      ( long "remove"
     <> help "If a specified file is in the index but is missing then it's removed. Default behavior is to ignore removed file." )
   <*> switch
      ( long "refresh"
     <> help "Looks at the current index and checks to see if merges or updates are needed by checking stat() information." )
  -- <*> option auto
  --    ( long "cacheinfo"
  --   <> value NoCacheInfoParams
  --   <> metavar "<mode> <object> <path>"
  --   <> help "Looks at the current index and checks to see if merges or updates are needed by checking stat() information." )
  <*> many
      ( argument str (metavar "PATH"))

run :: Options -> IO ()
run _ = undefined
