{-# LANGUAGE DeriveGeneric #-}

module ContentIdentifier (
    create
  , createStrict
  , flattenIdList
  , nil
  , Algro (..)
  , Type
  , Value
  , toURNBuilder
  , toHexBuilder
  , toURN
  , ContentIdentifier (..)
    )
where

import           Control.Applicative
import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import qualified Crypto.Hash.SHA1         as SHA1
import qualified Crypto.Hash.SHA256       as SHA256
import qualified Crypto.Hash.SHA3         as SHA3
import qualified Data.ByteString          as B
import qualified Data.ByteString.Base16   as Base16
import qualified Data.ByteString.Base32   as Base32
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8    as C
import qualified Data.ByteString.Lazy     as L
import           Data.ByteString.Short    (ShortByteString, fromShort, toShort)
import qualified Data.ByteString.Short    as Short
import           Data.Digest.CRC32
import           Data.Monoid              ((<>))
import           Data.Serialize
import           Data.Word
import           GHC.Generics             (Generic)

data Algro = SHA1 | CRC32 | SHA256 | SHA3_256 | Nil deriving (Eq, Ord, Generic, Enum)
type Digest = ShortByteString
data ContentIdentifier = ContentIdentifier !Algro
                            {-# UNPACK #-} !Digest deriving (Eq, Ord, Generic)

type HashSize = Int
type Type = Algro
type Value = Digest

sha3_256HashLength :: HashSize
sha3_256HashLength = 256

createStrict :: Algro -> B.ByteString -> ContentIdentifier
createStrict a d = ContentIdentifier a $! toShort $! hash a d

create :: Algro -> L.ByteString -> ContentIdentifier
create a d = ContentIdentifier a $! toShort $! hashLazy a d

flattenIdList :: Algro -> [ContentIdentifier] -> ContentIdentifier
flattenIdList a = createStrict a . foldr combine B.empty where
  combine = B.append . fromShort . (\(ContentIdentifier _ d) -> d)

nil :: ContentIdentifier
nil = ContentIdentifier Nil Short.empty

hashLazy :: Algro -> L.ByteString -> B.ByteString
hashLazy SHA1     = SHA1.hashlazy
hashLazy SHA256   = SHA256.hashlazy
hashLazy SHA3_256 = SHA3.hashlazy sha3_256HashLength
hashLazy CRC32    = L.toStrict . toLazyByteString . word32BE . crc32
hashLazy Nil      = undefined

hash :: Algro -> B.ByteString -> B.ByteString
hash SHA1     = SHA1.hash
hash SHA256   = SHA256.hash
hash SHA3_256 = SHA3.hash sha3_256HashLength
hash CRC32    = L.toStrict . toLazyByteString . word32BE . crc32
hash Nil      = undefined

toURNBuilder :: ContentIdentifier -> Builder
toURNBuilder (ContentIdentifier algro digest) =
  urnPrefix algro <> byteString (Base32.encode $ fromShort digest)

toHexBuilder :: ContentIdentifier -> Builder
toHexBuilder (ContentIdentifier _ d) =
  byteString $! Base16.encode $ fromShort d

toHex :: ContentIdentifier -> B.ByteString
toHex (ContentIdentifier _ d) = Base16.encode $ fromShort d

toURN :: ContentIdentifier -> B.ByteString
toURN (ContentIdentifier a d) = urnPrefixForAlgro a `B.append` Base32.encode (fromShort d)

fromURN :: B.ByteString -> Either String ContentIdentifier
fromURN = undefined

urnPrefix :: Algro -> Builder
urnPrefix a = urn <> colon <> algro <> colon
  where
    urn = string8 "urn"
    colon = char8 ':'
    algro = string8 (algroName a)

urnPrefixForAlgro :: Algro -> B.ByteString
urnPrefixForAlgro algro =
  C.pack $ "urn:" ++ algroName algro ++ ":"

algroNameAsByteString :: Algro -> B.ByteString
algroNameAsByteString = C.pack . algroName

algroName :: Algro -> String
algroName CRC32 = "crc32"
algroName SHA1 = "sha1"
algroName SHA256 = "sha256"
algroName SHA3_256 = "sha3-256"
algroName Nil = "nil"

instance Show ContentIdentifier where
  show = C.unpack . toURN

instance Serialize Algro where
  put a = put ((fromIntegral $ fromEnum a) :: Word8)
  get = fmap (toEnum . fromIntegral) (get :: Get Word8)

instance Serialize ContentIdentifier where
  put (ContentIdentifier a d) = put a >> put d
  get = ContentIdentifier <$> get <*> get

instance Serialize ShortByteString where
  put = put . fromShort
  get = fmap toShort get

instance NFData Algro where rnf = genericRnf
instance NFData ContentIdentifier where rnf = genericRnf
