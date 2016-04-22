module Dupes.FileStat (FileStat, UnixFileStat(..), create, toByteString) where

import           Data.Binary
import qualified Data.ByteString                  as B
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           System.PosixCompat

newtype FileStat = FileStat B.ByteString
  deriving (Eq, Ord, Show)

instance FromField FileStat where
  fromField field = FileStat <$> fromField field

instance ToField FileStat where
  toField = toField . toByteString

data UnixFileStat =
       UnixFileStat
         { mtime :: EpochTime
         , ctime :: EpochTime
         , inode :: FileID
         , size  :: FileOffset
         , uid   :: UserID
         , gid   :: GroupID
         }

create :: FileStat
create = FileStat B.empty

fromUnixFileStat :: UnixFileStat -> FileStat
fromUnixFileStat = undefined

toByteString :: FileStat -> B.ByteString
toByteString (FileStat bs) = bs

instance Binary FileStat where
  put (FileStat bs) = put bs
  get = FileStat <$> get

prop_encode_decode_is_id = undefined
