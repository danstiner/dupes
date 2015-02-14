{-# LANGUAGE Rank2Types #-}

module Command.Keep (
    Options
  , parserInfo
  , run
) where

import           Command.Common
import           DuplicateCache
import           Repository                   as R

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.List
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude                as P
import           System.Directory

data Options = Options
  { optPaths :: [FilePath]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Discard all duplicates that have at least one copy in the specified pathspec's")

parser :: Parser Options
parser = Options
  <$> many
      ( argument str (metavar "PATHSPEC") )

run :: Options -> IO ()
run opt = keepDuplicatesUnder (optPaths opt)

keepDuplicatesUnder :: [FilePath] -> IO ()
keepDuplicatesUnder = mapM canonicalizePath >=> R.runEffect . keepDuplicatesUnderEffect

keepDuplicatesUnderEffect :: MonadResource m => [FilePath] -> RepositoryHandle -> Effect m ()
keepDuplicatesUnderEffect paths r = adapt r (findDuplicatesOutside paths) >-> printPaths

findDuplicatesOutside :: Monad m => [FilePath] -> Producer HashPath (DuplicateCacheT m) ()
findDuplicatesOutside paths = assert False undefined -- sequence_ $ map duplicatesUnderButOutside paths
  where
    pathIsAKeeper path = any (`isPrefixOf` path) paths
