{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Dupes.Actions (
    update,
    listAll,
    listDuplicates,
    removeInPaths,
    removeSuffixes,
    testGroup,
    DuplicateAction,
    ) where

import           Control.Monad
import           Data.List              (isPrefixOf)
import           Dupes.FileHash         (FileHash)
import           Dupes.Index            (Index)
import qualified Dupes.Index            as Index
import           Dupes.Repository
import           Dupes.WorkingDirectory
import           Lens.Family            (view)
import           PathSpec
import           Pipes
import           Pipes.Group
import qualified Pipes.Path             as Path
import qualified Pipes.Prelude          as P
import           Pipes.Safe
import           System.Directory       (removeFile)
import           System.FilePath

import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

data UpdatedIndexEntry = UpdatedIndexEntry FilePath
  deriving Show

update :: Repository -> Producer UpdatedIndexEntry (SafeT IO) ()
update repository = hoist liftBase $ Index.withIndex (indexPath repository) $ \index ->
  walk (workingDirectory repository) >->
  P.filter isFileEntry >->
  P.map Path.getPath >->
  P.map (makeRelativePath (workingDirectory repository)) >->
  P.mapM (\path -> Index.updateFile index path >> return (UpdatedIndexEntry path))
  where
    isFileEntry (Path.FileEntry _ _) = True
    isFileEntry _ = False

listAll :: Repository -> Producer FilePath (SafeT IO) ()
listAll repository = Index.withIndex (indexPath repository) Index.listAll

listDuplicates :: Repository -> Producer (FilePath, FileHash) (SafeT IO) ()
listDuplicates repository = Index.withIndex (indexPath repository) Index.listDuplicates

data DuplicateAction = KeepDuplicate FilePath
                     | RemoveDuplicate FilePath
  deriving Show

removeInPaths :: Repository -> [PathSpec] -> Bool -> Producer FilePath (SafeT IO) ()
removeInPaths repository pathspecs dryRun = Index.withIndex (indexPath repository)
                                              (removeInPaths' pathspecs dryRun)

removeInPaths' :: [PathSpec] -> Bool -> Index -> Producer FilePath (SafeT IO) ()
removeInPaths' pathspecs dryRun index = for hashesWithDuplicates removeForHash >->
                                        P.filter actionIsRemove >->
                                        P.map extractPath >->
                                        P.mapM removePath
  where
    actionIsRemove (RemoveDuplicate _) = True
    actionIsRemove _ = False
    extractPath (RemoveDuplicate path) = path
    removePath :: FilePath -> SafeT IO FilePath
    removePath path = unless dryRun (liftBase (removeFile path)) >> return path
    removeForHash :: FileHash -> Producer DuplicateAction (SafeT IO) ()
    removeForHash hash = do
      areAnyOutside <- lift $ anyOutside hash
      when areAnyOutside (removeAllMatchingFor hash)
    anyOutside :: FileHash -> (SafeT IO) Bool
    anyOutside hash = pAny' (not . pathSpecMatches) (listFilesWithHash hash)
    pAny' :: Monad m => (a -> Bool) -> Producer a m () -> m Bool
    pAny' f producer = P.fold (||) False id (producer >-> P.map f)
    removeAllMatchingFor :: FileHash -> Producer DuplicateAction (SafeT IO) ()
    removeAllMatchingFor hash = listFilesWithHash hash >-> removeProducerByPredicate pathSpecMatches
    pathSpecMatches path = any (`matches` path) pathspecs
    hashesWithDuplicates = Index.listHashesWithDuplicates index
    listFilesWithHash = Index.listFilesWithHash index

data DuplicateSet m a = DuplicateSet FileHash (Producer FilePath m a)

removeSuffixes :: Repository -> Producer DuplicateAction (SafeT IO) ()
removeSuffixes = undefined

data KeepChoice = KeepLeft
                | KeepRight
                | KeepBoth

removeSuffix :: FilePath -> FilePath -> KeepChoice
removeSuffix leftPath rightPath =
  if leftPath `baseNameIsPrefixOf` rightPath
    then KeepLeft
    else KeepBoth

removeByPredicate :: (a -> Bool) -> a -> a -> KeepChoice
removeByPredicate test left right
  | test left = KeepRight
  | test right = KeepLeft
  | otherwise = KeepBoth

removeProducerByPredicate :: Monad m => (FilePath -> Bool) -> Pipe FilePath DuplicateAction m ()
removeProducerByPredicate f = P.map
                                (\path -> if f path
                                            then RemoveDuplicate path
                                            else KeepDuplicate path)

removeMatching :: [PathSpec] -> FilePath -> FilePath -> KeepChoice
removeMatching pathspecs =
  removeByPredicate (\path -> any (`matches` path) pathspecs)

removeMatching'' :: Monad m => forall r. Producer FilePath m r -> Producer DuplicateAction m r
removeMatching'' = undefined

data DuplicateMatchResult a = DuplicateMatches a
                            | DoesNotMatch a

matches' :: a -> [PathSpec] -> DuplicateMatchResult a
matches' = undefined

{-|
remove :: Mode -> IO ()
remove (PathSpecs pathSpecs) = removeDupes . matchingAnyOf $ pathSpecs
  where
    matchingAnyOf :: [PathSpec] -> Condition
    matchingAnyOf = Any <$> map Matches
remove Suffixes = removeDupes' (Holds hasPrefixDupe)
  where
    hasPrefixDupe file = dupes file >>= lift . P.any (`fileBaseNameIsPrefixOf` file)
    fileBaseNameIsPrefixOf a b = getFilePath a `baseNameIsPrefixOf` getFilePath b
-}
prop_baseNameIsPrefixOf_nameWithSuffix :: FilePath -> String -> Bool
prop_baseNameIsPrefixOf_nameWithSuffix path suffix =
  (validPath ++ ".ext") `baseNameIsPrefixOf` (validPath ++ nameSuffix ++ ".ext")
  where
    validPath = filter (not . isExtSeparator) path
    nameSuffix = filter (not . isExtSeparator) $ filter (not . isPathSeparator) suffix

case_baseNameIsNotPrefixOf_differentDirectoriesSameName =
  False @=? "/parent.ext" `baseNameIsPrefixOf` "/parent/file.ext"

case_baseNameIsNotPrefixOf_differentExtensions =
  False @=? "file.ext" `baseNameIsPrefixOf` "file.ext2"

case_baseNameIsNotPrefixOf_multiSegmentExtension =
  False @=? "file.gz" `baseNameIsPrefixOf` "file.tar.gz"

baseNameIsPrefixOf :: FilePath -> FilePath -> Bool
baseNameIsPrefixOf path1 path2 =
  let (dir1, filename1) = splitFileName path1
      (dir2, filename2) = splitFileName path2
      (basename1, ext1) = splitExtensions filename1
      (basename2, ext2) = splitExtensions filename2
  in basename1 `isPrefixOf` basename2 && ext1 == ext2 && dir1 `equalFilePath` dir2

testGroup = $(testGroupGenerator)
