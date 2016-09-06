module Dupes.WorkingDirectory (WorkingDirectory, construct, walk, makeRelativePath) where

import           Pipes
import qualified Pipes.Path as Path
import qualified System.FilePath as FilePath

newtype WorkingDirectory = WorkingDirectory FilePath
  deriving Show

construct :: FilePath -> WorkingDirectory
construct = WorkingDirectory

walk :: MonadIO m => WorkingDirectory -> Producer Path.PathEntry m ()
walk (WorkingDirectory path) = Path.walk path

makeRelativePath :: WorkingDirectory -> FilePath -> FilePath
makeRelativePath (WorkingDirectory path) = FilePath.makeRelative path
