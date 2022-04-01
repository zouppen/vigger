{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Matrix where

import Data.Aeson
import Network.Curl
import Network.Curl.Aeson
import Control.Exception (handle, throw)
import qualified Network.URI.Encode as U
import Data.Text.Lazy (Text)

import Config

-- Some helpers for parsing the JSON input

newtype BaseUrl = BaseUrl String deriving (Show)

instance FromJSON BaseUrl where
  parseJSON x = fmap BaseUrl $ parseJSON x ... "m.homeserver" ... "base_url"

newtype RoomId = RoomId String deriving (Show)

instance FromJSON RoomId where
  parseJSON x = fmap RoomId $ parseJSON x ... "room_id"

newtype ContentUri = ContentUri String deriving (Show)

instance FromJSON ContentUri where
  parseJSON x = fmap ContentUri $ parseJSON x ... "content_uri"

-- |Matrix connection, holding some basic information such as the token.
data MatrixConn = MatrixConn { baseUrl  :: BaseUrl
                             , opts     :: [CurlOption]
                             } deriving (Show)

matrixQuery :: FromJSON a
  => MatrixConn    -- ^Matrix connection
  -> String        -- ^Request method
  -> String        -- ^Endpoint (starts with slash)
  -> Maybe Payload -- ^Payload, /jsonPayload something/, Nothing or vice versa.
  -> IO a          -- ^JSON response
matrixQuery (MatrixConn (BaseUrl baseUrl) opts) method endpoint payload =
  curlAesonRaw eitherDecode opts method (baseUrl <> endpoint) payload

-- |Initialize a Matrix connection
initMatrixConn :: Matrix -> IO MatrixConn
initMatrixConn Matrix{..} = do
  let opts = [ CurlHttpHeaders [ "Authorization: Bearer " <> accessToken ] ]
  baseUrl <- getServerBaseUrl homeserver
  pure MatrixConn{..}

-- |Get real matrix URL using well-known-URI. See
-- https://spec.matrix.org/latest/client-server-api/#well-known-uri
getServerBaseUrl :: URLString -> IO BaseUrl
getServerBaseUrl url = do
  -- Finding well-known if any
  bu@(BaseUrl realUrl) <- handle when404 $ curlAesonCustom
    [CurlFollowLocation True, CurlMaxRedirs 10]
    "GET"
    (url <> "/.well-known/matrix/client")
    noData
  -- Just for validation, the only thing we're interested is that it's
  -- JSON content.
  curlAesonGet (realUrl <> "/_matrix/client/versions") :: IO Value
  pure bu
  where when404 :: CurlAesonException -> IO BaseUrl
        when404 e@CurlAesonException{..} = do
          if curlCode == CurlHttpReturnedError
            then pure $ BaseUrl url -- Well-known ignored
            else throw e -- More fatal case

joinRoom :: MatrixConn -> String -> IO RoomId
joinRoom conn room =
  matrixQuery conn "POST" ("/_matrix/client/v3/join/" <> U.encode room) Nothing


upload :: MatrixConn -> String -> Payload -> IO ContentUri
upload conn room payload = do
  roomId <- joinRoom conn room
  matrixQuery conn "POST" "/_matrix/media/v3/upload" (Just payload)
