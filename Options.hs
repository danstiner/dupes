
module Options (
    execParserWithArgs
) where


import Control.Applicative (pure, (<$>), (<|>), (<**>))
import Data.Monoid (mempty, mconcat)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Options.Applicative.BashCompletion
import Options.Applicative.Builder hiding (briefDesc)
import Options.Applicative.Builder.Internal
import Options.Applicative.Common
import Options.Applicative.Help
import Options.Applicative.Extra
import Options.Applicative.Types
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParserWithArgs :: ParserInfo a -> [String] -> IO a
execParserWithArgs info args = customExecParserWithArgs (prefs idm) args info

-- | Run a program description with custom preferences.
customExecParserWithArgs :: ParserPrefs -> [String] -> ParserInfo a -> IO a
customExecParserWithArgs pprefs args pinfo = do
  case execParserPure pprefs pinfo args of
    Success a -> return a
    Failure failure -> do
      progn <- getProgName
      let (msg, exit) = execFailure failure progn
      case exit of
        ExitSuccess -> putStrLn msg
        _           -> hPutStrLn stderr msg
      exitWith exit
    CompletionInvoked compl -> do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
      exitWith ExitSuccess