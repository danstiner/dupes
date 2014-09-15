{-# LANGUAGE Rank2Types #-}

module Command.ParseUtil (
	pathspecSource
) where

import Control.Monad
import Control.Monad.Trans ( lift )
import Data.List ( delete )
import Data.Machine
import System.IO

pathspecSource :: [FilePath] -> Bool -> SourceT IO FilePath
pathspecSource argPaths readMoreFromStdin =
  if (readMoreFromStdin || elem stdinFilename argPaths)
    then stdinAsLinesSource ~> prepended filteredArgPaths
    else source filteredArgPaths
  where
    filteredArgPaths = delete stdinFilename argPaths
    stdinFilename = "-"

stdinAsLinesSource :: SourceT IO FilePath
stdinAsLinesSource = construct $ stdinLinesPlan
  where
    stdinLinesPlan = do
      eof <- lift isEOF
      unless eof $ do
        line <- lift getLine
        yield line
        stdinLinesPlan