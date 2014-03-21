module Logging (
	registerLogger
) where

import System.Log.Handler.Log4jXML as Log4j
import System.Log.Logger
import System.FilePath ( (</>) )

import qualified App

registerLogger :: FilePath -> Priority -> IO ()
registerLogger outfolder p = do
  s <- Log4j.log4jFileHandler' (outfolder </> "log.xml") p
  updateGlobalLogger rootLoggerName (setLevel p . setHandlers [s])

