# Expo Server SDK in Haskell

Work in progress. 

Feel free to grab it and modify to your needs.

## Operations

```hs
sendPushNotification :: PushMessage -> IO (Either String PushTicketResponse)
```

## Helpers

```hs
createMessageSimple :: PushToken -> Title -> Body -> PushMessage
```
## Expo Docs

More details on [how Expo API works](https://docs.expo.io/versions/latest/guides/push-notifications/#sending-notifications-from-your-server).

Example [Rust implementation](https://github.com/expo/expo-server-sdk-rust).
