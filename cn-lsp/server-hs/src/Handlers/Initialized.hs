{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Initialized (handler) where

import Language.LSP.Protocol.Message (Method (..), SMethod (..), TNotificationMessage)
import Language.LSP.Server (Handlers, notificationHandler)
import Log (cInfo)
import Monad (ServerM)

handler :: Handlers ServerM
handler = notificationHandler SMethod_Initialized doInitialized

doInitialized :: TNotificationMessage 'Method_Initialized -> ServerM ()
doInitialized _notif =
  do
    cInfo "Server initialized"
