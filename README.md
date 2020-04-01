# Expo Server SDK in Haskell

Work in progress. 

Feel free to grab it and modify to your needs.

## Operations

```hs
sendPushNotification :: PushMessage -> IO ByteString
```

## Helpers

```hs
createMessageSimple :: PushToken -> Title -> Body -> PushMessage
```
