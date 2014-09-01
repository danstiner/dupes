
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
import Data.Serialize
import qualified Blob

import qualified ContentIdentifier as CI

type Id = CI.Id

type Mode = Int
type Filename = String
data Entry = Entry Mode Filename Blob.Id deriving (Eq, Show)

type Entries = [Entry]

data Tree = Tree Id Entries deriving (Show, Eq)

instance Serialize Tree where
    put (Tree ident entries) = do
        put ident
        forM_ entries put
    get = do
        get

instance Serialize Entry where
    put (Entry mode filename key) = do
        put mode
        put filename
        put key
    get = do
        get

create :: Entries -> Tree
create entries = Tree hash entries
    where
        hash = CI.create CI.SHA3_256 $ encode entries

createEntry :: Filename -> Mode -> Blob.Id -> Entry
createEntry f m i = Entry m f i

toBlob :: Tree -> Blob.Blob
toBlob = Blob.create . encode
