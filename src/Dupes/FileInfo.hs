module Dupes.FileInfo (FileInfo) where

import           Dupes.FileHash
import           System.PosixCompat

data FileInfo =
       FileInfo
         { mtime    :: EpochTime
         , ctime    :: EpochTime
         , inode    :: FileID
         , size     :: FileOffset
         , uid      :: UserID
         , gid      :: GroupID
         , fileHash :: FileHash
         }
  deriving (Eq, Ord, Show)
