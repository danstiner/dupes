{-# LANGUAGE Rank2Types #-}

module Command.Keep (
    Options
  , parserInfo
  , run
) where

import           Command.ParseUtil
import           Store.Repository      as R

import           Options.Applicative

data Options = Options
  { optStdin :: Bool
  , optPaths :: [FilePath]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Discard all duplicates that have at least one copy in the specified pathspec's")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "stdin"
     <> help "Read pathspec's from STDIN" )
  <*> many
      ( argument str (metavar "PATHSPEC") )

run :: Options -> IO ()
run _ = undefined
