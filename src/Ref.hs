
module Ref (
    Ref(Ref)
  , Id
  , create
  , createId
  , toString
) where

import qualified Blob

newtype Id = String String deriving (Eq, Show, Ord)

data Ref = Ref Id Blob.Id deriving (Show, Eq)

createId :: String -> Ref.Id
createId = String

create :: Id -> Blob.Id -> Ref
create = Ref

toString :: Id -> String
toString (String s) = s
