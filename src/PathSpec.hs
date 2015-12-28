{-# LANGUAGE TemplateHaskell #-}

module PathSpec (PathSpec, PathSpecString, matches, parse, pureTestGroup) where

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

case_matches_simple_asterisk_glob = assert (matches (parse "foo/*") "foo/bar")

pureTestGroup = $(testGroupGenerator)