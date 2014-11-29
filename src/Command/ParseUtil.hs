{-# LANGUAGE Rank2Types #-}

module Command.ParseUtil (
    pathspecSource
  , PathSpec
) where

import Control.Monad
import Control.Monad.Trans ( lift )
import Data.List ( delete )
import Data.Machine
import System.IO

type PathSpec = FilePath

pathspecSource :: [PathSpec] -> Bool -> SourceT IO PathSpec
pathspecSource argPaths readMoreFromStdin =
  if readMoreFromStdin || elem stdinFilename argPaths
    then stdinAsLinesSource ~> prepended filteredArgPaths
    else source filteredArgPaths
  where
    filteredArgPaths = delete stdinFilename argPaths
    stdinFilename = "-"

stdinAsLinesSource :: SourceT IO String
stdinAsLinesSource = construct stdinLinesPlan where
  stdinLinesPlan = do
    eof <- lift isEOF
    unless eof $ do
      line <- lift getLine
      yield line
      stdinLinesPlan
