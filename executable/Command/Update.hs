{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.Update (Options, parserInfo, run) where

import           Strings

import           Data.String.Interpolate
import qualified Dupes.Actions           as Actions
import qualified Dupes.Repository        as Repository
import           Logging
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude           as P
import           Pipes.Safe
import           System.Directory

data Options = Options { }

parserInfo :: ParserInfo Options
parserInfo = info parser (progDesc "Update index to match working directory")

parser :: Parser Options
parser = pure Options

run :: Options -> IO ()
run _ = getCurrentDirectory >>= Repository.findFrom >>= updateOrExit
  where
    updateOrExit x =
      case x of
        Just repository -> update repository
        Nothing -> exitErrorM $(logTag)
                     [i|Neither path nor any of its parents have a #{appName} index|]
    update path = runSafeT . runEffect $ Actions.update path >-> printUpdateEntry
    printUpdateEntry = P.print
