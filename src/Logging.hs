module Logging (
    logLefts
  , register
  , module System.Log.Logger
) where

import           Data.Either               as Either
import           System.IO
import           System.Log.Handler.Simple as SimpleLog
import           System.Log.Logger

register :: Priority -> IO ()
register p = do
  std <- stderrLogger p
  updateGlobalLogger rootLoggerName (setLevel p . setHandlers [std])

stderrLogger :: Priority -> IO (GenericHandler Handle)
stderrLogger = streamHandler stderr

logLefts :: String -> Priority -> [Either String a] -> IO [a]
logLefts logname pri xs =
  mapM_ (logM logname pri) ls >> return rs
  where
    (ls, rs) = Either.partitionEithers xs
