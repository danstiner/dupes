module Telemetry (
    recordLaunch
  , recordExit
  , recordLsDupes
  , register
  , telemetryM
) where

import Control.Monad.Trans
import System.FilePath ( (</>) )
import System.Log.Handler.Log4jXML as Log4j
import System.Log.Logger

data Event =
    Boot
  | Exit
  | LsDupes { dupeBucketCount :: Int }
  deriving ( Show )

type Datapoint = String

level :: Priority
level = INFO

tag :: String
tag = "Telemetry"

register :: FilePath -> IO ()
register folder = do
  s <- Log4j.log4jFileHandler' filePath level
  updateGlobalLogger tag (setLevel level . setHandlers [s])
  where
    filePath = folder </> "telemetry.xml"

recordEvent :: Event -> IO ()
recordEvent = execTelemetry . record

recordLaunch :: IO ()
recordLaunch = recordEvent Boot

recordExit :: IO ()
recordExit = recordEvent Exit

recordLsDupes :: Int -> IO ()
recordLsDupes = recordEvent . LsDupes

telemetryM :: (Show v) => Datapoint -> v -> IO ()
telemetryM n v = infoM tag $ n ++ ": " ++ (show v)

newtype Telemetry m a = Telemetry { runTelemetry :: m a}

class (Monad m) => TelemetryMonad m where
  record :: Event -> Telemetry m ()

execTelemetry :: (Monad m) => Telemetry m a -> m a
execTelemetry = runTelemetry

instance TelemetryMonad IO where
  record = lift . (infoM tag) . show

instance MonadTrans Telemetry where
  lift = Telemetry
