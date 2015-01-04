{-# LANGUAGE Rank2Types #-}

module Command.ParseUtil (
    PathSpec
) where

import           Control.Monad
import           Control.Monad.Trans (lift)
import           Data.List           (delete)
import           System.IO

type PathSpec = FilePath
