module File (
    File (..)
  ) where

newtype File = File { getFilePath :: FilePath } deriving (Show)
