
-- Listing of a directory
module Tree (
    Tree ( Tree )
  , Entry ( Entry )
  , Id
  , create
  , createEntry
  , toBlob
) where

import Control.Monad ( forM_ )
import Crypto.Hash.SHA3 ()
import qualified Blob
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified ContentIdentifier as CI

type HashDigest = B.ByteString

type Id = CI.Id

type Mode = Int
type Filename = String
data Entry = Entry Mode Filename Blob.Id deriving (Eq, Show)

type Entries = [Entry]

data Tree = Tree Id Entries deriving (Show, Eq)

instance Binary.Binary Tree where
    put (Tree ident entries) = do
        Binary.put ident
        forM_ entries Binary.put
    get = do
        Binary.get

instance Binary.Binary Entry where
    put (Entry mode filename key) = do
        Binary.put mode
        Binary.put filename
        Binary.put key
    get = do
        Binary.get

create :: Entries -> Tree
create entries = Tree hash entries
    where
        hash = CI.create CI.SHA3_256 $ L.toStrict $ Binary.encode entries

createEntry :: Filename -> Mode -> Blob.Id -> Entry
createEntry f m i = Entry m f i

toBlob :: Tree -> Blob.Blob
toBlob = Blob.create . L.toStrict . Binary.encode
