module Command.Ls (
    Options
  , parserInfo
  , run
) where

import           DuplicateCache
import           Index
import           Repository                   as R

import           Control.Monad.Trans.Resource
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude                as P

data Options = Options
  { optAll :: Bool }

parser :: Parser Options
parser = Options
  <$> switch
      ( long "all"
     <> help "Show not just duplicate files." )

parserInfo :: ParserInfo Options
parserInfo = info parser
  (progDesc "Show information about files in the duplicate index")

run :: Options -> IO ()
run (Options {optAll=True}) = R.runEffect printIndex
run (Options {optAll=False}) = R.runEffect printDuplicates

printIndex :: RepositoryHandle -> Effect (ResourceT IO) ()
printIndex repository = listIndexEntries >-> getEntryPath >-> P.print
  where
    listIndexEntries = Index.list $ getIndex repository
    getEntryPath = P.map indexEntryPath

printDuplicates :: RepositoryHandle -> Effect (ResourceT IO) ()
printDuplicates repository = listDuplicates >-> toString >-> P.print
  where
    listDuplicates = DuplicateCache.list (getCache repository)
    toString = P.map stringify
    stringify (HashPath hash path) = show hash ++ "\t" ++ path
