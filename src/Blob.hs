
-- Content addressed blobs
module Blob (
    Blob(Blob)
  , Id
  , Data (..)
  , create
  , createId
  , recreateLazy
  , toHexString
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified ContentIdentifier as CI

idType :: CI.Type
idType = CI.SHA3_256

type Id = CI.ContentIdentifier
data Data = Bytes B.ByteString | LazyBytes L.ByteString deriving (Show, Eq)

data Blob = Blob Id Data deriving (Show, Eq)

create :: B.ByteString -> Blob
create s = createFromData $! Bytes s

createFromData :: Data -> Blob
createFromData dat = Blob (createId dat) dat

createId :: Data -> Blob.Id
createId d = case d of
  Bytes dat -> CI.create idType dat
  LazyBytes dat -> CI.createLazy idType dat

recreateLazy :: Blob.Id -> L.ByteString -> Blob
recreateLazy i d = Blob i (LazyBytes d)

toHexString :: CI.ContentIdentifier -> String
toHexString = show
