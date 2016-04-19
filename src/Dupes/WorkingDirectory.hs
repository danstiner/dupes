module Dupes.WorkingDirectory (WorkingDirectory, construct, walk) where

import           Pipes
import qualified Pipes.Path as Path

newtype WorkingDirectory = WorkingDirectory FilePath
  deriving Show

construct :: FilePath -> WorkingDirectory
construct = WorkingDirectory

walk :: MonadIO m => WorkingDirectory -> Producer Path.PathEntry m ()
walk (WorkingDirectory path) = Path.walk path
