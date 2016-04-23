module Dupes.Actions (update) where

import           Dupes.Index            as Index
import           Dupes.Repository
import           Dupes.WorkingDirectory
import           Pipes
import qualified Pipes.Path             as Path
import qualified Pipes.Prelude          as P
import           Pipes.Safe

data UpdatedIndexEntry = UpdatedIndexEntry FilePath
  deriving Show

update :: Repository -> Producer UpdatedIndexEntry (SafeT IO) ()
update repository = hoist liftBase $ withIndex (indexPath repository) $ \index ->
  walk (workingDirectory repository) >->
  P.filter isFileEntry >->
  P.map Path.getPath >->
  P.mapM (\path -> Index.updateFile index path >> return (UpdatedIndexEntry path))
  where
    isFileEntry (Path.FileEntry _ _) = True
    isFileEntry _ = False
