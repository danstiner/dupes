module PathSpec (
    PathSpec
  , PathSpecString
  , matches
  , parse
) where

import           Data.List
import           System.FilePath
import qualified System.FilePath.Glob as Glob

type PathSpecString = String

data PathSpec = PathSpec { directoryPrefix :: FilePath, glob :: Glob.Pattern } deriving (Show)

parse :: PathSpecString -> PathSpec
parse = PathSpec "" . Glob.compile

matches :: PathSpec -> FilePath -> Bool
matches spec path =
  maybe False (Glob.match (glob spec)) $ stripPrefix (directoryPrefix spec) (normalise path)
