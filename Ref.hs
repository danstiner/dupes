
module Ref (
    Ref(Ref)
  , Id
  , create
) where

import qualified Blob

newtype Id = String String deriving (Eq, Show, Ord)

data Ref = Ref Id Blob.Id deriving (Show, Eq)

create :: Id -> Blob.Id -> Ref
create name refid = Ref name refid
