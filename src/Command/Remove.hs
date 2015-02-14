{-# LANGUAGE Rank2Types #-}

module Command.Remove (
    Options
  , parserInfo
  , run
) where

import           Command.Common
import           DuplicateCache
import           Repository                   as R

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Free
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

data Mode
  = Prefixes
  | Paths [FilePath]

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
run opt@(Options {optPrefixes=True})  = run' opt Prefixes
run opt@(Options {optPrefixes=False}) = run' opt $ Paths (optPaths opt)

run' :: Options -> Mode -> IO ()
run' _ Prefixes = R.runEffect printPrefixesWithDuplicates
run' _ (Paths paths) = printPathsWithDuplicatesOutside paths

printPrefixesWithDuplicates :: MonadResource m => RepositoryHandle -> Effect m ()
printPrefixesWithDuplicates handle = f handle >-> printPaths

f :: MonadResource m => RepositoryHandle -> Producer HashPath m ()
f handle = assert False undefined -- a' $ a handle prefixesWithDuplicates

-- a :: (DuplicateCacheC c, MonadResource m) => RepositoryHandle -> Producer HashPath c () -> Producer HashPath m ()
-- a handle = runFOnDB (getCache handle)

a' :: Monad m => m (Producer HashPath m ()) -> Producer HashPath m ()
a' action = do
  result <- lift $ action
  result

prefixesWithDuplicates :: DuplicateCacheC m => Producer HashPath m ()
prefixesWithDuplicates = DuplicateCache.listC >-> filterDuplicates' isPrefixWithDuplicates
  where
    isPrefixWithDuplicates source = P.any (source `hashPathIsPrefixOf`)
    hashPathIsPrefixOf a b = getFilePath a `pathIsPrefixOf` getFilePath b
    pathIsPrefixOf p1 p2 =
      let (name1,ext1) = splitExtension p1
          (name2,ext2) = splitExtension p2 in
        name1 `isPrefixOf` name2 && ext1 == ext2

printPathsWithDuplicatesOutside :: [FilePath] -> IO ()
printPathsWithDuplicatesOutside = mapM canonicalizePath >=> R.runEffect . printPathsWithDuplicatesOutsideEffect

printPathsWithDuplicatesOutsideEffect :: MonadResource m => [FilePath] -> RepositoryHandle -> Effect m ()
printPathsWithDuplicatesOutsideEffect paths r = f'' (f' paths r) >-> printPaths

f'' :: MonadResource m => m (Producer HashPath m ()) -> Producer HashPath m ()
f'' action = do
  result <- lift action
  result

f' :: MonadResource m => [FilePath] -> RepositoryHandle -> m (Producer HashPath m ())
f' paths r = return $ hoist (adapt' r) $ filesWithDupesOutside paths

adapt'' :: MonadResource m => RepositoryHandle -> DuplicateCacheT m (Producer HashPath m ()) -> m (Producer HashPath m ())
adapt'' = adapt'

-- l :: Monad m => t m (u m a)


filesWithDupesOutside :: Monad m => [FilePath] ->  Producer HashPath (DuplicateCacheT m) ()
filesWithDupesOutside paths = mapM_ filesUnderPathWithDupesOutside paths
  -- assert False undefined -- return $ sequence_ things
  where
    filesUnderPathWithDupesOutside :: Monad m => FilePath -> Producer HashPath (DuplicateCacheT m) ()
    filesUnderPathWithDupesOutside path = do
      list <- lift $ DuplicateCache.listPathF path
      hoist lift list >-> filterDuplicates hasDupesOutside
      -- assert False undefined -- return (list >-> filterDupe)
    hasDupesOutside _ = P.any (isOutside . getFilePath)
    isOutside path = all (not . (`isPrefixOf` path)) paths
