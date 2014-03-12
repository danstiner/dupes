
-- Listing of directory
module Tree (
    Tree(Tree)
  , TreeEntry(TreeEntry)
  , Id
  , create
  , createEntry
  , toBlob
) where

import Crypto.Hash.SHA3 ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as Binary
import qualified Blob

type HashDigest = B.ByteString

data Id = None | Sha3 HashDigest deriving (Eq, Show, Ord)

type Mode = Int
type Filename = String
data TreeEntry = TreeEntry Mode Filename Blob.Id deriving (Eq, Show)

type Data = [TreeEntry]

data Tree = Tree Id Data deriving (Show, Eq)

instance Binary.Binary Tree where
	put (Tree id dat) = do
		Binary.put "arst"
	get = do
		Binary.get

create :: Data -> Tree
create dat = Tree None dat

createEntry :: Filename -> Mode -> Blob.Id -> TreeEntry
createEntry name mode ident = TreeEntry mode name ident

toBlob :: Tree -> Blob.Blob
toBlob tree =
  Blob.create $ L.toStrict $ Binary.encode tree
