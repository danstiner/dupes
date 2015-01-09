{-# LANGUAGE Rank2Types #-}

module Command.Remove (
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
  { optPrefixes :: Bool
  , optPaths :: [FilePath]  }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Delete duplicates")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "prefix"
     <> help "Remove duplicate whose names names are prefixes of each other" )
  <*> many
      ( argument str (metavar "PATHSPEC") )

run :: Options -> IO ()
run opt = if optPrefixes opt
  then removePrefixes
  else mapM_ (\p -> canonicalizePath p >>= dedupePath) (optPaths opt)

removePrefixes :: IO ()
removePrefixes = do
  r <- R.get
  runResourceT $ R.withRepository r $ runEffect . removePrefixesEffect

removePrefixesEffect :: MonadResource m => RepositoryHandle -> Effect m ()
removePrefixesEffect r = DuplicateCache.list (getCache r) >-> filterPrefixes (getCache r) >-> printPath

filterPrefixes :: MonadResource m => DuplicateCache -> Pipe HashPath HashPath m ()
filterPrefixes r = forever $ await >>= go
  where
    go entry@(HashPath hash path) = do
      prefix <- lift $ P.any (`filenameIsPrefixOf` path) (listDupes r hash >-> P.map (\(HashPath _ p) -> p) >-> P.filter (/= path))
      when prefix (yield entry)
    filenameIsPrefixOf p1 p2 =
      let (name1,ext1) = splitExtension p1
          (name2,ext2) = splitExtension p2 in
        name1 `isPrefixOf` name2 && ext1 == ext2

printPath :: MonadIO m => Consumer HashPath m ()
printPath = forever $ await >>= p
  where
    p (HashPath hash path) = liftIO $ putStrLn path

dedupePath :: FilePath -> IO ()
dedupePath path = do
  r <- R.get
  runResourceT $ R.withRepository r $ runEffect . dedupePathEffect path

dedupePathEffect :: MonadResource m => FilePath ->  RepositoryHandle -> Effect m ()
dedupePathEffect path r = DuplicateCache.listPath (getCache r) path >-> P.filterM (hasDupesOutside (getCache r) path) >-> printPath

hasDupesOutside :: MonadResource m => DuplicateCache -> FilePath -> HashPath -> m Bool
hasDupesOutside r listedPath (HashPath hash path) = fmap not $ P.null (list >-> filterToOutside)
  where
    list = listDupes r hash
    filterToOutside = P.filter (\(HashPath _ p) -> p /= path && not (listedPath `isPrefixOf` p))