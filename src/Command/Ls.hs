module Command.Ls (
    Options
  , parserInfo
  , run
) where

import Index as Index
import           Store.Repository      as R

import           Control.Monad.Trans.Resource
import Control.Monad
import Pipes
import           Options.Applicative

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
run (Options {_optAll=False}) = undefined

printIndex :: IO ()
printIndex = do
  r <- R.get
  runResourceT $ R.withRepository r $ runEffect . printIndexEffect

printIndexEffect :: (MonadResource m) => RepositoryHandle -> Effect m ()
printIndexEffect r = Index.list (getIndex r) >-> printIndexEntry

printIndexEntry :: MonadIO m => Consumer IndexEntry m ()
printIndexEntry = forever $ await >>= p
  where
    p (IndexEntry path) = liftIO $ putStrLn path
