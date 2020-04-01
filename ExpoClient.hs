{-# LANGUAGE TemplateHaskell            #-}

module ExpoClient
  ( PushMessage(..)
  , PushToken(..)
  , Title(..)
  , Body(..)
  , Priority(..)
  , Sound(..)
  , sendPushNotification
  , createMessageSimple
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Text

import           Network.HTTP.Req

expoHost :: Text
expoHost = "exp.host"

-- | Types
-- -----------------------------------------------------------------------------

newtype PushToken = PushToken Text
newtype Title = Title Text
newtype Body = Body Text
newtype Ttl = Ttl Integer
newtype Expiration = Expiration Integer
newtype Badge = Badge Integer
data Sound = SoundDefault
data Priority = PriorityDefault | PriorityNormal | PriorityHigh

data PushMessage = PushMessage
  { _to         :: PushToken
  , _data       :: Maybe Value
  , _title      :: Maybe Title
  , _body       :: Maybe Body
  , _sound      :: Maybe Sound
  , _ttl        :: Maybe Ttl
  , _expiration :: Maybe Expiration
  , _prority    :: Maybe Priority
  , _badge      :: Maybe Badge
  }

-- | Codecs
-- -----------------------------------------------------------------------------

instance ToJSON Sound where
  toJSON SoundDefault = "default"

instance ToJSON Priority where
  toJSON PriorityDefault = "default"
  toJSON PriorityNormal  = "normal"
  toJSON PriorityHigh    = "high"


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

createMessageSimple :: PushToken -> Title -> Body -> PushMessage
createMessageSimple to' title' body' = PushMessage
  { _to         = to'
  , _data       = Nothing
  , _title      = Just title'
  , _body       = Just body'
  , _sound      = Nothing
  , _ttl        = Nothing
  , _expiration = Nothing
  , _prority    = Nothing
  , _badge      = Nothing
  }

-- | Operations
-- -----------------------------------------------------------------------------

sendRequest :: ByteString -> IO ByteString
sendRequest b = runReq defaultHttpConfig $ do
  let url = https expoHost /: "--" /: "api" /: "v2" /: "push" /: "send"
  let options = header "Accept" "application/json"
             <> header "Accept-Encoding" "gzip, deflate"
             <> header "Content-Type" "application/json"
  responseBody <$> req POST url (ReqBodyLbs b) lbsResponse options

sendPushNotification :: PushMessage -> IO ByteString
sendPushNotification = sendRequest . encode

