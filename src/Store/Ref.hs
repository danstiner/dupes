module Store.Ref (
    RefStore (..)
) where

import Ref
import qualified Control.Monad.Trans.State as State

class RefStore s where
    read :: Id -> (State.StateT s IO) (Maybe Ref)
    set :: Ref -> State.StateT s IO ()
    delete :: Id -> State.StateT s IO ()