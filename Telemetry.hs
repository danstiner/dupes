module Telemetry (
	  logTag
	, registerLogger
	, telemetryM
) where

import System.Log.Handler.Log4jXML as Log4j
import System.Log.Logger
import System.FilePath ( (</>) )

type Datapoint = String

registerLogger :: FilePath -> IO ()
registerLogger outfolder = do
  s <- Log4j.log4jFileHandler' (outfolder </> "telemetry.xml") INFO
  updateGlobalLogger logTag (setLevel INFO . setHandlers [s])

logTag :: String
logTag = "Telemetry"

telemetryM :: (Show v) => Datapoint -> v -> IO ()
telemetryM n v = infoM logTag $ n ++ ": " ++ (show v)
