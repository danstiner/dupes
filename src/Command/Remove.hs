{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE Rank2Types #-}

module Command.Remove (
    Options
  , parserInfo
  , run
  , htf_thisModulesTests
) where

import           Dedupe
import           PathSpec

import           Control.Applicative
import           Data.List           (isPrefixOf)
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude       as P
import           System.FilePath

import           Test.Framework

data Options = Options
  { optSuffixes  :: Bool
  , optPathSpecs :: [PathSpecString]  }

data Mode
  = Suffixes
  | PathSpecs [PathSpec]

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Delete specified files which are duplicates")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "suffixes"
     <> help "Remove files whose name is a suffix of a duplicate in the same directory" )
  <*> many
      ( argument str (metavar "PATHSPEC") )

run :: Options -> IO ()
run     (Options {optSuffixes=True})  = remove Suffixes
run opt@(Options {optSuffixes=False}) = remove . PathSpecs $ parsePathSpecs opt

parsePathSpecs :: Options -> [PathSpec]
parsePathSpecs = map PathSpec.parse . optPathSpecs

remove :: Mode -> IO ()
remove (PathSpecs pathSpecs) = removeDupes . matchingAnyOf $ pathSpecs
  where
    matchingAnyOf :: [PathSpec] -> Condition
    matchingAnyOf = Any <$> map Matches
remove Suffixes = removeDupes' (Holds hasPrefixDupe)
  where
    hasPrefixDupe file = dupes file >>= lift . P.any (`fileBaseNameIsPrefixOf` file)
    fileBaseNameIsPrefixOf a b = getFilePath a `baseNameIsPrefixOf` getFilePath b

prop_baseNameIsPrefixOf_nameWithSuffix :: FilePath -> String -> Bool
prop_baseNameIsPrefixOf_nameWithSuffix path suffix =
    (validPath ++ ".ext") `baseNameIsPrefixOf` (validPath ++ nameSuffix ++ ".ext")
  where
    validPath = filter (not . isExtSeparator) path
    nameSuffix = filter (not . isExtSeparator) $ filter (not . isPathSeparator) suffix

test_baseNameIsNotPrefixOf_differentDirectoriesSameName =
  assertBool $ not ("/parent.ext" `baseNameIsPrefixOf` "/parent/file.ext")

test_baseNameIsNotPrefixOf_differentExtensions =
  assertBool $ not ("file.ext" `baseNameIsPrefixOf` "file.ext2")

test_baseNameIsNotPrefixOf_multiSegmentExtension =
  assertBool $ not ("file.gz" `baseNameIsPrefixOf` "file.tar.gz")

baseNameIsPrefixOf :: FilePath -> FilePath -> Bool
baseNameIsPrefixOf path1 path2 =
  let (dir1, filename1) = splitFileName path1
      (dir2, filename2) = splitFileName path2
      (basename1, ext1) = splitExtensions filename1
      (basename2, ext2) = splitExtensions filename2 in
    basename1 `isPrefixOf` basename2 && ext1 == ext2 && dir1 `equalFilePath` dir2
