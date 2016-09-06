{-# LANGUAGE QuasiQuotes #-}

module Strings (appName, noIndexFoundErrorMessage) where

import           Data.String.Interpolate

appName :: String
appName = "dupes"

noIndexFoundErrorMessage :: String
noIndexFoundErrorMessage = [i|Neither the current directory nor any of its parents have a #{appName} index|]
