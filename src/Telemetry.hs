module Telemetry (
    record
  , register
  , Event (..)
) where

import Control.Monad.Trans
import Data.IORef
import Data.UUID
import Data.UUID.V4 as UUIDv4
import System.FilePath ( (</>) )
import System.IO.Unsafe ( unsafePerformIO )
import System.Log.Handler.Log4jXML as Log4j
import System.Log.Logger

data Event =
    Boot
  | Exit
  deriving ( Show )

newtype SessionId = SessionId UUID deriving ( Show )

data Record = Record { _event :: Event, _session :: SessionId } deriving ( Show )

level :: Priority
level = INFO

tag :: String
tag = "Telemetry"

filename :: String
filename = "telemetry.xml"

processSessionId :: IORef SessionId
{-# NOINLINE processSessionId #-}
processSessionId = unsafePerformIO $ do
  uuid <- UUIDv4.nextRandom
  newIORef $ SessionId uuid

register :: FilePath -> IO ()
register folder = do
  s <- Log4j.log4jFileHandler' filePath level
  updateGlobalLogger tag (setLevel level . setHandlers [s])
  where
    filePath = folder </> filename

record :: TelemetryMonad m => Event -> m ()
record = execTelemetry . emit

getSessionId :: IO SessionId
getSessionId = readIORef processSessionId

newtype TelemetryT m a = TelemetryT { runTelemetryT :: m a}

class (Monad m) => TelemetryMonad m where
  emit :: Event -> TelemetryT m ()

execTelemetry :: (Monad m) => TelemetryT m a -> m a
execTelemetry = runTelemetryT

instance TelemetryMonad IO where
  emit event = lift decorated >>= lift . (infoM tag) . show
    where
      decorated = do
        sessId <- getSessionId
        return $ Record event sessId

instance Monad m => Monad (TelemetryT m) where
  fail = TelemetryT . fail
  return = lift . return
  x >>= f = TelemetryT $ runTelemetryT x >>= runTelemetryT . f

instance MonadTrans TelemetryT where
  lift = TelemetryT
