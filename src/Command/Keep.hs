{-# LANGUAGE Rank2Types #-}

module Command.Keep (
    Options
  , parserInfo
  , run
) where

import           Dedupe
import           PathSpec

import           Options.Applicative

data Options = Options
  { optPathSpecs :: [PathSpecString]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Discard all duplicates that have at least one copy in the specified pathspec(s)")

parser :: Parser Options
parser = Options
  <$> many
      ( argument str (metavar "PATHSPEC") )

run :: Options -> IO ()
run = keep . map PathSpec.parse . optPathSpecs

keep :: [PathSpec] -> IO ()
keep = removeDupes . notMatching
  where
    notMatching :: [PathSpec] -> Condition
    notMatching = All <$> map (Not . Matches)
