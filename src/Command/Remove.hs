{-# LANGUAGE Rank2Types #-}

module Command.Remove (
    Options
  , parserInfo
  , run
) where

import           DuplicateCache
import           Repository                   as R

import           Control.Applicative
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
  { optPrefixes :: Bool
  , optPaths    :: [FilePath]  }

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
  else mapM_ (canonicalizePath >=> dedupePath) (optPaths opt)

removePrefixes :: IO ()
removePrefixes = R.runEffect removePrefixesEffect

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
dedupePath path = R.runEffect $ dedupePathEffect path

dedupePathEffect :: MonadResource m => FilePath ->  RepositoryHandle -> Effect m ()
dedupePathEffect path r = DuplicateCache.listPath (getCache r) path >-> P.filterM (hasDupesOutside (getCache r) path) >-> printPath

hasDupesOutside :: MonadResource m => DuplicateCache -> FilePath -> HashPath -> m Bool
hasDupesOutside r listedPath (HashPath hash path) = not <$> P.null (list >-> filterToOutside)
  where
    list = listDupes r hash
    filterToOutside = P.filter (\(HashPath _ p) -> p /= path && not (listedPath `isPrefixOf` p))
