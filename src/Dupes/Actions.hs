module Dupes.Actions (update) where

import           Dupes.Repository
import           Dupes.WorkingDirectory
import           Pipes
import qualified Pipes.Path             as Path
import qualified Pipes.Prelude          as P
import           Pipes.Safe

data UpdatedIndexEntry = UpdatedIndexEntry FilePath
  deriving Show

update :: Repository -> Producer UpdatedIndexEntry (SafeT IO) ()
update repository = walk (workingDirectory repository) >-> P.map (UpdatedIndexEntry . Path.getPath)
