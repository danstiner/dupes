module Logging (
	registerLogger
) where

import System.FilePath ( (</>) )
import System.IO
import System.Log.Handler.Log4jXML as Log4j
import System.Log.Handler.Simple as SimpleLog
import System.Log.Logger

registerLogger :: FilePath -> Priority -> IO ()
registerLogger outfolder p = do
  app <- Log4j.log4jFileHandler' (outfolder </> "log.xml") NOTICE
  std <- stderrLogger p
  updateGlobalLogger rootLoggerName (setLevel p . setHandlers [app, std])


stderrLogger :: Priority -> IO (GenericHandler Handle)
stderrLogger p = streamHandler stderr p
