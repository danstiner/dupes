{-# LANGUAGE Rank2Types #-}

module Command.Keep (
    Options
  , parserInfo
  , run
) where

import           DuplicateCache
import           Repository                   as R

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
run opt = mapM_ (canonicalizePath >=> keepPath) (optPaths opt)

keepPath :: FilePath -> IO ()
keepPath path = R.runEffect $ keepPathEffect path
  where
    keepPathEffect :: MonadResource m => FilePath ->  RepositoryHandle -> Effect m ()
    keepPathEffect path r = dupesOf (getCache r) path (DuplicateCache.listPath (getCache r) path) >-> printPath

dupesOf :: MonadResource m => DuplicateCache -> FilePath -> Producer HashPath m () -> Producer HashPath m ()
dupesOf r pathToKeep p = for p body
  where
    body (HashPath hash path) = listDupes r hash >-> P.filter (\(HashPath _ p) -> p /= path && not (pathToKeep `isPrefixOf` p))

printPath :: MonadIO m => Consumer HashPath m ()
printPath = P.map getFilePath >-> P.print
