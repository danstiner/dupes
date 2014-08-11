module Store.Blob (
  BlobStore (..)
) where

import qualified Blob
import qualified Control.Monad.Trans.State as State

class BlobStore s where
  get :: Blob.Id -> (State.StateT s IO) (Maybe Blob.Blob)
  put :: Blob.Blob -> State.StateT s IO ()
  delete :: Blob.Id -> State.StateT s IO ()
