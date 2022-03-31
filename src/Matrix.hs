{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Matrix where

import Data.Aeson
import Network.Curl
import Network.Curl.Aeson
import Control.Exception (handle, throw)

newtype BaseUrl = BaseUrl String

instance FromJSON BaseUrl where
  parseJSON x = fmap BaseUrl $ parseJSON x ... "m.homeserver" ... "base_url"

-- |Get real matrix URL using well-known-URI. See
-- https://spec.matrix.org/latest/client-server-api/#well-known-uri
getServer :: URLString -> IO String
getServer url = do
  -- Finding well-known if any
  BaseUrl realUrl <- handle when404 $ curlAesonCustom
    [CurlFollowLocation True, CurlMaxRedirs 10]
    "GET"
    (url <> "/.well-known/matrix/client")
    noData
  -- Just for validation, the only thing we're interested is that it's
  -- JSON content.
  curlAesonGet (realUrl <> "/_matrix/client/versions") :: IO Value
  pure realUrl
  where when404 :: CurlAesonException -> IO BaseUrl
        when404 e@CurlAesonException{..} = do
          if curlCode == CurlHttpReturnedError
            then pure $ BaseUrl url -- Well-known ignored
            else throw e -- More fatal case

