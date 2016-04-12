module Pipes.Path (PathEntry(..), walk, getPath, getStatus) where

import           Control.Exception
import qualified Data.ByteString.Char8 as C
import           Data.Either
import           Data.List
import           Data.Ord              (comparing)
import           Pipes
import           System.Directory
import           System.FilePath
import           System.IO
import           System.PosixCompat.Files

data PathEntry = FileEntry FilePath FileStatus
               | DirectoryStart FilePath
               | DirectoryEnd FilePath FileStatus

getPath :: PathEntry -> FilePath
getPath (FileEntry path _) = path
getPath (DirectoryStart path) = path
getPath (DirectoryEnd path _) = path

getStatus :: PathEntry -> FileStatus
getStatus (FileEntry _ status) = status
getStatus (DirectoryStart _) = assert False undefined
getStatus (DirectoryEnd _ status) = status

walk :: MonadIO m => FilePath -> Producer PathEntry m ()
walk path = do
  result <- liftIO (try (getSymbolicLinkStatus path) :: IO (Either SomeException FileStatus))
  case result of
    Left ex      -> liftIO $ hPrint stderr ex
    Right status -> recurse (path, status)

recurse :: MonadIO m => (FilePath, FileStatus) -> Producer PathEntry m ()
recurse (path, status)
  | isDirectory status = do
      yield (DirectoryStart path)
      result <- liftIO ((try $ getDirectoryContents path) :: IO (Either SomeException [FilePath]))
      case result of
        Left ex -> liftIO $ hPrint stderr ex
        Right contents -> do
          let normalContents = map (path </>) $ filter (not . isPrefixOf ".") $ filter
                                                                                  (`notElem` [ "."
                                                                                             , ".."
                                                                                             ])
                                                                                  contents
          (errors, statuses) <- fmap partitionEithers . liftIO $ mapM tryGetStatusWithPath' normalContents
          mapM_ printEx errors
          mapM_ recurse . sortBy comparePaths $ map addDirSlash statuses
      dirStatusEither <- liftIO $ tryGetStatus path
      case dirStatusEither of
        Left error -> printEx error
        Right dirStatus -> yield (DirectoryEnd path dirStatus)
  | otherwise = yield (FileEntry path status)
  where
    printEx = liftIO . hPrint stderr
    addDirSlash (path', status') = if isDirectory status'
                                     then (path' ++ "/", status')
                                     else (path', status')
    comparePaths = comparing (C.pack . fst)

tryGetStatus :: FilePath -> IO (Either SomeException FileStatus)
tryGetStatus = try . getSymbolicLinkStatus

tryGetStatusWithPath' :: FilePath -> IO (Either SomeException (FilePath, FileStatus))
tryGetStatusWithPath' path = do
  status <- tryGetStatus path
  return (fmap (\s -> (path, s)) status)
