{-# LANGUAGE OverloadedStrings #-}

module Store.RemoteHttp (
    createStore
  , Store
) where

import Store.Blob
import Store.Ref
import qualified Blob
import qualified Ref

import Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text

import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Class (lift)

import Network.HTTP.Conduit

type Url = String
type ParamName = String
type ParamValue = String

newtype Store = Store Url

data RefResponse = RefResponse Text Text

instance BlobStore Store where
    get key = do
        store <- State.get
        let url = buildUrl store "blob" (Blob.toString key)
        response <- lift $ simpleHttp url
        return $ Just $ Blob.Blob key (Blob.LazyBytes response)

    put (Blob.Blob key _) = do
        store <- State.get
        let url = buildUrl store "blob" (Blob.toString key) 
        lift $ simpleHttp url
        return ()

    delete key = do
        store <- State.get
        let url = buildUrl store "blob" (Blob.toString key)
        lift $ simpleHttp url
        return ()

instance RefStore Store where
    read name = do
        store <- State.get
        let url = buildUrl store "ref" (Ref.toString name)
        response <- lift $ simpleHttp url
        return $ makeRef name (Aeson.decode response)

    set (Ref.Ref name _) = do
        store <- State.get
        let url = buildUrl store "ref" (Ref.toString name)
        lift $ simpleHttp url
        return ()

    delete name = do
        store <- State.get
        let url = buildUrl store "ref" (Ref.toString name)
        lift $ simpleHttp url
        return ()

instance Aeson.FromJSON RefResponse where
    parseJSON (Object obj) = do
        name <- obj .: "name"
        value <- obj .: "value"
        return $ RefResponse name value

makeRef :: Ref.Id -> Maybe RefResponse -> Maybe Ref.Ref
makeRef key response = 
    case response of
        Nothing -> Nothing
        Just (RefResponse _ value) -> Just $ Ref.Ref key (Blob.createIdFromHex $ Text.unpack value)

buildUrl :: Store -> ParamName -> ParamValue -> Url
buildUrl (Store base) name value =
    base ++ "/" ++ name ++ "/" ++ value

createStore :: Url -> Store
createStore url = Store url
