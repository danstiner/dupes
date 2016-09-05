{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.List (Options, parserInfo, run) where

import           Data.String.Interpolate
import qualified Dupes.Actions           as Actions
import           Dupes.Repository        (Repository)
import qualified Dupes.Repository        as Repository
import           Logging
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude           as P
import           Pipes.Safe
import           Strings
import           System.Directory

data Options = Options { optAll :: Bool }

parser :: Parser Options
parser = Options
         <$> switch (long "all"
                     <> help "Show all indexed files, not just duplicates.")

parserInfo :: ParserInfo Options
parserInfo = info parser (progDesc "Show information about files in the duplicate index")

run :: Options -> IO ()
run options = getCurrentDirectory >>= Repository.findFrom >>= maybe errorNoIndex (list options)
  where
    errorNoIndex = exitErrorM $(logTag)
                     [i|Neither the current directory nor any of its parents have a #{appName} index|]

list :: Options -> Repository -> IO ()
list (Options { optAll = True }) = printIndex
list (Options { optAll = False }) = printDuplicates

printIndex :: Repository -> IO ()
printIndex repository = runSafeT . runEffect $ Actions.listAll repository >-> P.print

printDuplicates :: Repository -> IO ()
printDuplicates repository = runSafeT . runEffect $ Actions.listDuplicates repository >-> P.print
