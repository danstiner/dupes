
-- Listing of directory
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

type HashDigest = B.ByteString

data Id = None | Sha3 HashDigest deriving (Eq, Show, Ord)

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

instance Binary.Binary Id where
	put None = do
		Binary.put (0 :: Binary.Word8)
	get = do
		tag <- Binary.getWord8
		case tag of
			0 -> return None

create :: Entries -> Tree
create = Tree None

createEntry :: Filename -> Mode -> Blob.Id -> Entry
createEntry name mode ident = Entry mode name ident

toBlob :: Tree -> Blob.Blob
toBlob tree =
  Blob.create $ L.toStrict $ Binary.encode tree
