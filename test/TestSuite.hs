{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Command.Remove
import {-@ HTF_TESTS @-} Dedupe
import {-@ HTF_TESTS @-} Repository

main :: IO ()
main = htfMain htf_importedTests
