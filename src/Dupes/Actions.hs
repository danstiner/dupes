module Dupes.Actions (update) where

import           Dupes.Repository
import           Pipes
import           Pipes.Safe

data UpdatedIndexEntry = UpdatedIndexEntry

update :: Repository -> Producer UpdatedIndexEntry (SafeT IO) ()
update = undefined
