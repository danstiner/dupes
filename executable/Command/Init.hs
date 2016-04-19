{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.Init (Options, parserInfo, run) where

import           Data.String.Interpolate
import           Dupes.Repository        as Repository
import           Logging
import           Options.Applicative
import           System.Directory

data Options = Options { }

parserInfo :: ParserInfo Options
parserInfo = info parser (progDesc "Create a repository")

parser :: Parser Options
parser = pure Options

run :: Options -> IO ()
run _ = getCurrentDirectory >>= findFromOrInitialize >>= printResult
  where
    printResult repository = noticeM $(logTag) [i|Repository at: #{show repository}|]

findFromOrInitialize :: FilePath -> IO Repository
findFromOrInitialize path = findFrom path >>= maybe (initialize path) return
