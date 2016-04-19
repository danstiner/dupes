module Dupes.Index (Index, construct) where

newtype Index = Index { unIndex :: FilePath }
  deriving Show

construct :: FilePath -> Index
construct = Index
