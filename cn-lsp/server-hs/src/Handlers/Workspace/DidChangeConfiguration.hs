{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Handlers.Workspace.DidChangeConfiguration (handler) where

import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as Text
import Language.LSP.Protocol.Message (Method (..), SMethod (..), TNotificationMessage (..))
import Language.LSP.Protocol.Types (DidChangeConfigurationParams)
import Language.LSP.Server (Handlers, notificationHandler)
import Log (sDebug)
import Monad (ServerM)

handler :: Handlers ServerM
handler = notificationHandler SMethod_WorkspaceDidChangeConfiguration doDidChangeConfiguration

doDidChangeConfiguration ::
  TNotificationMessage 'Method_WorkspaceDidChangeConfiguration ->
  ServerM ()
doDidChangeConfiguration notif =
  do
    sDebug $ Text.toStrict (Aeson.encodeToLazyText params)
  where
    params :: DidChangeConfigurationParams
    params = notif._params
