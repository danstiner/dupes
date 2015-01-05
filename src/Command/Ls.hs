module Command.Ls (
    Options
  , parserInfo
  , run
) where

import           DuplicateCache
import           Index
import           Store.Repository             as R

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Options.Applicative
import           Pipes

data Options = Options
  { _optAll :: Bool }

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the duplicate index")

parser :: Parser Options
parser = Options
  <$> switch
      ( long "all"
     <> help "Show not just duplicate files." )

run :: Options -> IO ()
run (Options {_optAll=True}) = printIndex
run (Options {_optAll=False}) = printDuplicates

printIndex :: IO ()
printIndex = do
  r <- R.get
  runResourceT $ R.withRepository r $ runEffect . printIndexEffect

printIndexEffect :: (MonadResource m) => RepositoryHandle -> Effect m ()
printIndexEffect r = Index.list (getIndex r) >-> printIndexEntry

printIndexEntry :: MonadIO m => Consumer IndexEntry m ()
printIndexEntry = forever $ await >>= p
  where
    p (IndexEntry path _) = liftIO $ putStrLn path

printDuplicates :: IO ()
printDuplicates = do
  r <- R.get
  runResourceT $ R.withRepository r $ runEffect . printDuplicatesEffect

printDuplicatesEffect :: (MonadResource m) => RepositoryHandle -> Effect m ()
printDuplicatesEffect r = DuplicateCache.list (getCache r) >-> printDuplicateEntry

printDuplicateEntry :: MonadIO m => Consumer HashPath m ()
printDuplicateEntry = forever $ await >>= p
  where
    p (HashPath hash path) = liftIO $ putStrLn (show hash ++ "\t" ++ path)
