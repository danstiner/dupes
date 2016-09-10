{-# LANGUAGE TemplateHaskell #-}

module Store (update, testGroup) where

import           Dupes.Repository (Repository)

import           Test.Tasty.TH

data UpdateResult = UpdateResult
  deriving Show

update :: Repository -> IO UpdateResult
update = undefined

testGroup = $(testGroupGenerator)
