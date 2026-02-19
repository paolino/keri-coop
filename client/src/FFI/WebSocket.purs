module FFI.WebSocket
  ( WebSocket
  , create
  , send
  , onMessage
  , onOpen
  , onClose
  , close
  ) where

import Prelude

import Effect (Effect)

foreign import data WebSocket :: Type

foreign import createImpl :: String -> Effect WebSocket

foreign import sendImpl :: WebSocket -> String -> Effect Unit

foreign import onMessageImpl :: WebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import onOpenImpl :: WebSocket -> Effect Unit -> Effect Unit

foreign import onCloseImpl :: WebSocket -> Effect Unit -> Effect Unit

foreign import closeImpl :: WebSocket -> Effect Unit

create :: String -> Effect WebSocket
create = createImpl

send :: WebSocket -> String -> Effect Unit
send = sendImpl

onMessage :: WebSocket -> (String -> Effect Unit) -> Effect Unit
onMessage = onMessageImpl

onOpen :: WebSocket -> Effect Unit -> Effect Unit
onOpen = onOpenImpl

onClose :: WebSocket -> Effect Unit -> Effect Unit
onClose = onCloseImpl

close :: WebSocket -> Effect Unit
close = closeImpl
