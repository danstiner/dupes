{-# LANGUAGE OverloadedStrings #-}

module Tests.Store.LevelDBExternal (externalTests) where

import           Store.LevelDB
import           Tests.Dupes             (storeOpContract)

import qualified Data.ByteString.Char8   as C
import           Database.LevelDB.Higher as Level
import           System.IO.Temp

import           Test.Framework

keySpace :: KeySpace
keySpace = "Tests.Store.LevelDB"

externalTests :: [Test]
externalTests = storeOpContract evalStoreOp
  where
    evalStoreOp actions = withSystemTempDirectory "Tests.Store.LevelDB" $ \dir ->
      Level.runCreateLevelDB dir keySpace $ storeOpToDBAction actions
