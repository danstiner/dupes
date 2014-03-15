{-# LANGUAGE OverloadedStrings #-}

module Store.RemoteHttp (
    createStore
  , Store
) where

import Store.Blob
import Store.Ref
import qualified Blob
import qualified Ref

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Lazy.Char8 (pack)

import System.Directory as Directory
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)

import Network.HTTP.Conduit

type Url = String
type ParamName = String
type ParamValue = String

newtype Store = Store Url

data RefResponse = RefResponse { name :: Text, value :: Text }
newtype SetResponse = SetResponse { result :: Text }

instance BlobStore Store where
	get key = do
		store <- State.get
		let url = buildUrl store "blob" (Blob.toString key)
		response <- lift $ simpleHttp url
		return $ Just $ Blob.Blob key (Blob.LazyBytes response)

	put (Blob.Blob key val) = do
		store <- State.get
		let url = buildUrl store "blob" (Blob.toString key) 
		response <- lift $ simpleHttp url -- val
		return ()

	delete key = do
		store <- State.get
		let url = buildUrl store "blob" (Blob.toString key)
		response <- lift $ simpleHttp url
		return ()

instance RefStore Store where
	read name = do
		store <- State.get
		let url = buildUrl store "ref" (Ref.toString name)
		response <- lift $ simpleHttp url
		return $ makeRef name (Aeson.decode response)

	set (Ref.Ref name value) = do
		store <- State.get
		let url = buildUrl store "ref" (Ref.toString name)
		response <- lift $ simpleHttp url -- value
		return ()

	delete name = do
		store <- State.get
		let url = buildUrl store "ref" (Ref.toString name)
		response <- lift $ simpleHttp url
		return ()

instance Aeson.FromJSON RefResponse where
	parseJSON (Object obj) = do
		name <- obj .: "name"
		value <- obj .: "value"
		return $ RefResponse name value

makeRef :: Ref.Id -> Maybe RefResponse -> Maybe Ref.Ref
makeRef id response = 
	case response of
		Nothing -> Nothing
		Just (RefResponse name value) -> Just $ Ref.Ref id (Blob.createIdFromHex $ Text.unpack value)

buildUrl :: Store -> ParamName -> ParamValue -> Url
buildUrl (Store base) name value =
	base ++ "/" ++ name ++ "/" ++ value

createStore :: Url -> Store
createStore url = Store url
