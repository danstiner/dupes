module Command.List (Options, parserInfo, run) where

import           DuplicateCache
import           Index
import           Repository

import           Control.Monad.Trans.Resource
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude                as P

data Options = Options { optAll :: Bool }

parser :: Parser Options
parser = Options
         <$> switch (long "all"
                     <> help "Show all indexed files, not just duplicates.")

parserInfo :: ParserInfo Options
parserInfo = info parser (progDesc "Show information about files in the duplicate index")

run :: Options -> IO ()
run (Options { optAll = True }) = Repository.runEffect printIndex
run (Options { optAll = False }) = Repository.runEffect printDuplicates

printIndex :: RepositoryHandle -> Effect (ResourceT IO) ()
printIndex repository = listIndexEntries >-> getEntryPath >-> P.stdoutLn
  where
    listIndexEntries = Index.list $ getIndex repository
    getEntryPath = P.map indexEntryPath

printDuplicates :: RepositoryHandle -> Effect (ResourceT IO) ()
printDuplicates repository = listDuplicates >-> toString >-> P.stdoutLn
  where
    listDuplicates = DuplicateCache.list (getCache repository)
    toString = P.map stringify
    stringify (HashPath hash path) = show hash ++ "\t" ++ path
