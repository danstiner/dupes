module Logging (
    logLefts
  , register
) where

import Data.Either                 as Either
import System.FilePath             ((</>))
import System.IO
import System.Log.Handler.Log4jXML as Log4j
import System.Log.Handler.Simple   as SimpleLog
import System.Log.Logger

register :: FilePath -> Priority -> IO ()
register outfolder p = do
  app <- Log4j.log4jFileHandler' (outfolder </> "log.xml") NOTICE
  std <- stderrLogger p
  updateGlobalLogger rootLoggerName (setLevel p . setHandlers [app, std])

stderrLogger :: Priority -> IO (GenericHandler Handle)
stderrLogger = streamHandler stderr

logLefts :: String -> Priority -> [Either String a] -> IO [a]
logLefts logname pri xs =
  mapM_ (logM logname pri) ls >> return rs
  where
    (ls, rs) = Either.partitionEithers xs
