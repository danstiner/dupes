{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import           Database
import           Test.Framework

main :: IO ()
main = htfMain htf_importedTests
