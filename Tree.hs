
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
import Control.Monad ( forM_ )

type HashDigest = B.ByteString

data Id = None | Sha3 HashDigest deriving (Eq, Show, Ord)

type Mode = Int
type Filename = String
data TreeEntry = TreeEntry Mode Filename Blob.Id deriving (Eq, Show)

type Data = [TreeEntry]

data Tree = Tree Id Data deriving (Show, Eq)

instance Binary.Binary Tree where
	put (Tree ident entries) = do
		Binary.put ident
		forM_ entries Binary.put
	get = do
		Binary.get

instance Binary.Binary TreeEntry where
	put (TreeEntry mode filename key) = do
		Binary.put mode
		Binary.put filename
		Binary.put key
	get = do
		Binary.get

instance Binary.Binary Id where
	put None = do
		Binary.put (0 :: Binary.Word8)
	get = do
		tag <- Binary.getWord8
		case tag of
			0 -> return None

create :: Data -> Tree
create dat = Tree None dat

createEntry :: Filename -> Mode -> Blob.Id -> TreeEntry
createEntry name mode ident = TreeEntry mode name ident

toBlob :: Tree -> Blob.Blob
toBlob tree =
  Blob.create $ L.toStrict $ Binary.encode tree
