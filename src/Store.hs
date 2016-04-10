{-# LANGUAGE TemplateHaskell #-}

module Store (update, pureTests) where

import           Repository    (Repository)

import           Test.Tasty.TH

data UpdateResult = UpdateResult
  deriving Show

update :: Repository -> IO UpdateResult
update = undefined

pureTests = $(testGroupGenerator)
