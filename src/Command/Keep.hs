{-# LANGUAGE Rank2Types #-}

module Command.Keep (
    Options
  , parserInfo
  , run
) where

import           Store.Repository    as R
import DuplicateCache

import           Options.Applicative
import Pipes
import qualified Pipes.Prelude as P
import Data.List
import Control.Monad
import           Control.Monad.Trans.Resource
import Control.Exception
import System.FilePath.Posix
import System.Directory

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
run opt = mapM_ (\p -> canonicalizePath p >>= keepPath) (optPaths opt)

keepPath :: FilePath -> IO ()
keepPath path = do
  r <- R.get
  runResourceT $ R.withRepository r $ runEffect . keepPathEffect path

keepPathEffect :: MonadResource m => FilePath ->  RepositoryHandle -> Effect m ()
keepPathEffect path r = dupesOf (getCache r) path (DuplicateCache.listPath (getCache r) path) >-> printPath

dupesOf :: MonadResource m => DuplicateCache -> FilePath -> Producer HashPath m () -> Producer HashPath m ()
dupesOf r keepPath p = for p body
  where
    body entry@(HashPath hash path) = listDupes r hash >-> P.filter (\(HashPath _ p) -> p /= path && not (keepPath `isPrefixOf` p))

printPath :: MonadIO m => Consumer HashPath m ()
printPath = forever $ await >>= p
  where
    p (HashPath hash path) = liftIO $ putStrLn path
