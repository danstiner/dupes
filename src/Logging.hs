module Logging (
    logLefts,
    register,
    logTag,
    setQuietLogging,
    exitErrorM,
    module System.Log.Logger,
    ) where

import           Data.Either                as Either
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Exit
import           System.IO
import           System.Log.Handler.Simple  as SimpleLog
import           System.Log.Logger

setQuietLogging :: Bool -> IO ()
setQuietLogging True = registerQuiet
setQuietLogging False = register

register :: IO ()
register = registerAtPriority NOTICE

registerQuiet :: IO ()
registerQuiet = registerAtPriority WARNING

registerAtPriority :: Priority -> IO ()
registerAtPriority p = do
  std <- stderrLogger p
  updateGlobalLogger rootLoggerName (setLevel p . setHandlers [std])

stderrLogger :: Priority -> IO (GenericHandler Handle)
stderrLogger = streamHandler stderr

logLefts :: String -> Priority -> [Either String a] -> IO [a]
logLefts logname pri xs =
  mapM_ (logM logname pri) ls >> return rs
  where
    (ls, rs) = Either.partitionEithers xs

exitErrorM :: String -> String -> IO ()
exitErrorM logTag message = errorM logTag message >> exitFailure

logTag :: Q Exp
logTag = (LitE . StringL) <$> fmap loc_module qLocation
