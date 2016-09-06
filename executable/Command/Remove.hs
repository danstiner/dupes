{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.Remove (Options, parserInfo, run) where

import           Control.Applicative
import           Data.String.Interpolate
import qualified Dupes.Actions           as Actions
import           Dupes.Repository        (Repository)
import qualified Dupes.Repository        as Repository
import           Logging
import           Options.Applicative
import           PathSpec
import           Pipes
import qualified Pipes.Prelude           as P
import           Pipes.Safe
import           Strings
import           System.Directory

data Options = Options { optSuffixes :: Bool, optDryRun :: Bool, optPathSpecs :: [PathSpecString] }

parserInfo :: ParserInfo Options
parserInfo = info parser (progDesc "Delete specified files which are duplicates")

parser :: Parser Options
parser = Options <$> switch
                       (long "suffixes"
                        <> help
                             "Remove files whose name is a suffix of a duplicate in the same directory.")
                 <*> switch
                       (long "dry-run"
                        <> short 'n'
                        <> help "Don’t actually remove anything, just show what would be done.")
                 <*> many (argument str (metavar "PATHSPEC"))

run :: Options -> IO ()
run options = getCurrentDirectory >>= Repository.findFrom >>= maybe errorNoIndex (remove options)
  where
    errorNoIndex = exitErrorM $(logTag)
                     [i|Neither the current directory nor any of its parents have a #{appName} index|]

remove :: Options -> Repository -> IO ()
remove opt@(Options { optSuffixes = True }) repository = runSafeT . runEffect $ Actions.removeSuffixes
                                                                                  repository >-> P.print
remove opt@(Options { optSuffixes = False }) repository = runSafeT . runEffect $ Actions.removeInPaths
                                                                                   repository
                                                                                   pathSpecs
                                                                                   (optDryRun opt) >-> P.stdoutLn
  where
    pathSpecs = parsePathSpecs opt
    parsePathSpecs :: Options -> [PathSpec]
    parsePathSpecs = map PathSpec.parse . optPathSpecs
