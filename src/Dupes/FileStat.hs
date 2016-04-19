module Dupes.FileStat (FileStat) where

import           System.PosixCompat

data FileStat =
       FileStat
         { mtime :: EpochTime
         , ctime :: EpochTime
         , inode :: FileID
         , size  :: FileOffset
         , uid   :: UserID
         , gid   :: GroupID
         }
  deriving (Eq, Ord, Show)
