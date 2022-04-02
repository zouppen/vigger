{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}
module Matrix where

import Control.Exception (handle, throw)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import Data.Text.Lazy (Text)
import Data.UUID (toString)
import Data.UUID.V1 (nextUUID)
import GHC.Generics
import Network.Curl hiding (contentType)
import Network.Curl.Aeson
import qualified Network.URI.Encode as U

import Config
import Exceptions

-- Some helpers for parsing the JSON input

newtype BaseUrl = BaseUrl String deriving (Show)

instance FromJSON BaseUrl where
  parseJSON x = fmap BaseUrl $ parseJSON x ... "m.homeserver" ... "base_url"

newtype RoomId = RoomId String deriving (Show)

instance FromJSON RoomId where
  parseJSON x = fmap RoomId $ parseJSON x ... "room_id"

newtype ContentUri = ContentUri String deriving (Show, Generic)

instance FromJSON ContentUri where
  parseJSON x = fmap ContentUri $ parseJSON x ... "content_uri"

instance ToJSON ContentUri where
  toEncoding = genericToEncoding ourEncoding

newtype EventId = EventId String deriving (Show)

instance FromJSON EventId where
  parseJSON x = fmap EventId $ parseJSON x ... "event_id"

-- Helpers for JSON output

ourEncoding = defaultOptions{ omitNothingFields = True, unwrapUnaryRecords = True }

data Message = Message
  { body     :: Text
  , msgtype  :: Text
  , url      :: Maybe ContentUri
  , info     :: Maybe MessageInfo
  } deriving (Show, Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding ourEncoding

instance ToJSON MessageInfo where
  toEncoding = genericToEncoding ourEncoding

data MessageInfo = MessageInfo
  { mimetype :: String
  , size     :: Int64
  } deriving (Show, Generic)

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

-- |Upload content without sending them to any rooms
upload :: MatrixConn -> Payload -> IO ContentUri
upload conn payload =
  matrixQuery conn "POST" "/_matrix/media/v3/upload" (Just payload)

-- |Upload and send
sendFile :: MatrixConn -> String -> Text -> Text -> Payload -> IO EventId
sendFile conn room body msgtype file = do
  RoomId roomId <- joinRoom conn room
  uuid <- nextUUID >>= maybe (throwIO $ ViggerNonFatal "UUID generation failed") pure
  -- First, upload the file
  url <- upload conn file
  -- Then send the actual event
  let Payload{..} = file
      message = Message { url = Just url
                        , info = Just MessageInfo { mimetype = contentType
                                                  , size = B.length payload
                                                  }
                        , ..
                        }
      msgUri = "/_matrix/client/v3/rooms/" <>
               U.encode roomId <>
               "/send/m.room.message/" <>
               U.encode (toString uuid)
  print $ jsonPayload message
  matrixQuery conn "PUT" msgUri (jsonPayload message)

sendImage :: MatrixConn -> String -> Text -> Payload -> IO EventId
sendImage conn room body file = sendFile conn room body "m.image" file
