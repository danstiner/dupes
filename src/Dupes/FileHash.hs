{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dupes.FileHash (hashFile, integrationTests) where

import           Control.Exception
import           Control.Monad
import           Crypto.Hash            as Hash
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy   as L
import           Data.Maybe
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           Test.Tasty.HUnit
import           Test.Tasty.TH

newtype FileHash = FileHash (Digest SHA1)
  deriving (Show, Eq, Ord)

hashFile :: FilePath -> IO (Either String FileHash)
hashFile path = (Right <$> fileHash) `catch` returnIOException
  where
    fileHash = withBinaryFile path ReadMode hashHandle
    hashHandle handle = (FileHash . hashlazy) <$!> L.hGetContents handle
    returnIOException :: IOException -> IO (Either String FileHash)
    returnIOException = return . Left . show

case_hashFile_empty_file = withSystemTempFile "template" $ \path handle -> do
  hClose handle
  result <- hashFile path
  Right sha1OfEmptyFile @=? result
  where
    sha1OfEmptyFile = fileHashFromHexString "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    fileHashFromHexString = FileHash . fromJust . digestFromByteString . fst . Base16.decode

integrationTests = $(testGroupGenerator)
