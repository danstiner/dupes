{-# LANGUAGE Rank2Types #-}

module Command.Keep (
    Options
  , parserInfo
  , run
) where

import           DuplicateCache
import           Store.Repository             as R

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.List
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude                as P
import           System.Directory
import           System.FilePath.Posix

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
