{-# LANGUAGE TemplateHaskell #-}

module PathSpec (
    PathSpec,
    PathSpecString,
    matches,
    parse,
    pureTestGroup,
    ) where

import           Data.List
import           System.FilePath
import qualified System.FilePath.Glob as Glob

import           Test.Tasty.HUnit
import           Test.Tasty.TH

type PathSpecString = String

data PathSpec = PathSpec { directoryPrefix :: FilePath, glob :: Glob.Pattern }
  deriving Show

parse :: PathSpecString -> PathSpec
parse = PathSpec "" . Glob.compile

matches :: PathSpec -> FilePath -> Bool
matches spec path =
  maybe False (Glob.match (glob spec)) $ stripPrefix (directoryPrefix spec) (normalise path)

case_matches_exact_path = assert (matches (parse "foo/bar") "foo/bar")

case_star_matches_only_in_directory = do
  assert (matches (parse "foo/*") "foo/bar")
  assert $ not (matches (parse "foo/*") "baz/bar")

case_double_star_crosses_directory_boundaries = assert (matches (parse "**/*") "foo/bar/baz")

case_matches_fails_for_negated_path = assert $ not (matches (parse "~foo/bar") "foo/bar")

pureTestGroup = $(testGroupGenerator)
