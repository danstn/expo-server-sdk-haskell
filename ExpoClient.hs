{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module ExpoClient
  ( PushMessage(..)
  , PushToken(..)
  , PushTicket(..)
  , PushTicketResponse(..)
  , Error(..)
  , Status(..)
  , Title(..)
  , Body(..)
  , Priority(..)
  , Sound(..)
  , sendPushNotification
  , sendPushNotificationBatch
  , createMessageSimple
  ) where

import           GHC.Exts

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Text

import           Network.HTTP.Req

expoHost :: Text
expoHost = "exp.host"

-- | Push Message
--------------------------------------------------------------------------------

data PushMessage = PushMessage
  { _to                   :: PushToken
  , _data                 :: Maybe Value
  , _title                :: Maybe Title
  , _body                 :: Maybe Body
  , _sound                :: Maybe Sound
  , _ttl                  :: Maybe Ttl
  , _expiration           :: Maybe Expiration
  , _prority              :: Maybe Priority
  , _badge                :: Maybe Badge
  , __displayInForeground :: Maybe Bool
  }

newtype PushToken = PushToken Text
  deriving Show

newtype Title = Title Text

newtype Body = Body Text

newtype Ttl = Ttl Integer

newtype Expiration = Expiration Integer

newtype Badge = Badge Integer

data Sound = SoundDefault

data Priority = PriorityDefault
              | PriorityNormal
              | PriorityHigh

instance ToJSON Sound where
  toJSON SoundDefault = "default"

instance ToJSON Priority where
  toJSON PriorityDefault = "default"
  toJSON PriorityNormal  = "normal"
  toJSON PriorityHigh    = "high"


-- | Push Ticket
--------------------------------------------------------------------------------

data PushTicket = PushTicket
  { _status       :: Status
  , _ticketId     :: Maybe TicketId
  , _error        :: Maybe Error
  , _errorMessage :: Maybe Text
  } deriving Show

instance FromJSON PushTicket where
  parseJSON = withObject "PushTicket" $ \o -> do
    _status       <- o .: "status"
    _ticketId     <- o .:? "id"
    _errorMessage <- o .:? "message"
    details <- o .:? "details"
    _error <-
      case details of
        Just v  -> v .: "error"
        Nothing -> pure Nothing
    pure PushTicket{..}

data Status = StatusOk
            | StatusError
            deriving (Show, Eq)

instance FromJSON Status where
  parseJSON v@(String s) = case s of
    "ok"    -> pure StatusOk
    "error" -> pure StatusError
    _       -> unexpected v
  parseJSON v = typeMismatch "String" v

newtype TicketId = TicketId Text
  deriving (Show, FromJSON)

data Error = DeviceNotRegistered
           | MessageTooBig
           | MessageRateExceeded
           | InvalidCredentials
           | UnknownError Text
           deriving (Eq, Show)

instance FromJSON Error where
  parseJSON = withText "Error" $ pure . \case
    "DeviceNotRegistered" -> DeviceNotRegistered
    "MessageTooBig"       -> MessageTooBig
    "MessageRateExceeded" -> MessageRateExceeded
    "InvalidCredentials"  -> InvalidCredentials
    s                     -> UnknownError s

-- | Push ticket response format
-- https://docs.expo.io/versions/latest/guides/push-notifications/#push-ticket-format
-- {
--   "data": [
--     {
--       "status": "error" | "ok",
--       "id": string, // this is the Receipt ID
--       // if status === "error"
--       "message": string,
--       "details": JSON
--     },
--     ...
--   ],
--   // only populated if there was an error with the entire request
--   "errors": [{
--     "code": number,
--     "message": string
--   }]
-- }
--------------------------------------------------------------------------------
data PushTicketResponse = PushTicketResponse
  { _responseData   :: [PushTicket]
  , _responseErrors :: [Error]
  } deriving Show

instance FromJSON PushTicketResponse where
  parseJSON = withObject "PushTicketResponse" $ \o -> do
    v <- o .: "data"
    _responseData <- case v of
      Array _  -> parseJSON  v
      Object _ -> parseJSON $ Array $ fromList [v]
      _        -> unexpected v
    _responseErrors <- o .:? "errors" .!= []
    pure PushTicketResponse{..}

data RequestError = RequestError
  { _errCode :: Integer
  , _errMsg  :: Text
  } deriving Show

instance FromJSON RequestError where
  parseJSON = withObject "RequestError" $ \o ->
    RequestError <$> o .: "code" <*> o .: "message"

-- | Generic Codecs
-- -----------------------------------------------------------------------------

$(deriveToJSON defaultOptions ''PushToken)
$(deriveToJSON defaultOptions ''Title)
$(deriveToJSON defaultOptions ''Body)
$(deriveToJSON defaultOptions ''Ttl)
$(deriveToJSON defaultOptions ''Expiration)
$(deriveToJSON defaultOptions ''Badge)
$(deriveToJSON defaultOptions
  { fieldLabelModifier = Prelude.drop 1
  , omitNothingFields = True
  } ''PushMessage)

-- | Helpers
-- -----------------------------------------------------------------------------

createMessageSimple :: Title -> Body -> PushToken -> PushMessage
createMessageSimple title' body' to' = PushMessage
  { _to                   = to'
  , _data                 = Nothing
  , _title                = Just title'
  , _body                 = Just body'
  , _sound                = Nothing
  , _ttl                  = Nothing
  , _expiration           = Nothing
  , _prority              = Nothing
  , _badge                = Nothing
  , __displayInForeground = Nothing
  }

-- | Operations
-- -----------------------------------------------------------------------------

sendRequest :: ByteString -> IO (Either String PushTicketResponse)
sendRequest b = runReq defaultHttpConfig $ do
  let url = https expoHost /: "--" /: "api" /: "v2" /: "push" /: "send"
  let options = header "Accept" "application/json"
             <> header "Accept-Encoding" "gzip, deflate"
             <> header "Content-Type" "application/json"
  bs <- responseBody <$> req POST url (ReqBodyLbs b) lbsResponse options
  pure $ eitherDecode bs

sendPushNotification :: PushMessage -> IO (Either String PushTicketResponse)
sendPushNotification = sendRequest . encode

sendPushNotificationBatch :: [PushMessage] -> IO (Either String PushTicketResponse)
sendPushNotificationBatch = sendRequest . encode
