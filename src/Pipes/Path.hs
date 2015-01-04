module Pipes.Path
    (
      PathEntry (..)
    , walk
    , getPath
    ) where

import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.List
import           Pipes
import           System.FilePath
import           System.IO
import           System.Posix.Files
import           Control.Exception
import           System.Directory
import Data.Monoid (mappend)
import Data.Ord (comparing)

data PathEntry
  = FileEntry FilePath FileStatus
  | DirectoryStart FilePath
  | DirectoryEnd FilePath FileStatus

getPath :: PathEntry -> FilePath
getPath (FileEntry path _) = path
getPath (DirectoryStart path) = path
getPath (DirectoryEnd path _) = path

walk :: MonadIO m => FilePath -> Producer PathEntry m ()
walk path = do
        result <- liftIO (try (getSymbolicLinkStatus path) :: IO (Either SomeException FileStatus))
        case result of
            Left ex -> liftIO $ hPrint stderr ex
            Right status -> recurse (path, status)

recurse :: MonadIO m => (FilePath, FileStatus) -> Producer PathEntry m ()
recurse (path, status)
    | isDirectory status = do
        result <- liftIO ((try $ getDirectoryContents path) :: IO (Either SomeException [FilePath]))
        case result of
            Left ex -> liftIO $ hPrint stderr ex
            Right contents -> do
                let normalContents = map (path </>) $ filter (not . isPrefixOf ".") $ filter (`notElem` [".", ".."]) contents
                (errors, statuses) <- fmap partitionEithers . liftIO $ mapM getStatus' normalContents
                mapM_ printEx errors
                mapM_ recurse . sortBy comparePaths $ map addDirSlash statuses
    | otherwise = yield (FileEntry path status)
  where
    printEx = liftIO . hPrint stderr
    addDirSlash (path, status) = if isDirectory status then (path ++ "/", status) else (path, status)
    comparePaths = comparing (C.pack . fst)

getStatus :: FilePath -> IO (Either SomeException FileStatus)
getStatus = try . getSymbolicLinkStatus

getStatus' :: FilePath -> IO (Either SomeException (FilePath, FileStatus))
getStatus' path = do
    status <- getStatus path
    return (fmap (\s -> (path, s)) status)
