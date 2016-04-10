module Store (update) where

import           Repository (Repository)

data UpdateResult = UpdateResult
  deriving Show

update :: Repository -> IO UpdateResult
update = undefined
