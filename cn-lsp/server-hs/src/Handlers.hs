{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module Handlers (mkHandlers) where

import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as Text
import Handlers.Custom.RunCN qualified
import Handlers.Initialized qualified
import Handlers.TextDocument.DidChange qualified
import Handlers.TextDocument.DidClose qualified
import Handlers.TextDocument.DidOpen qualified
import Handlers.TextDocument.DidSave qualified
import Handlers.Workspace.DidChangeConfiguration qualified
import Language.LSP.Protocol.Message
  ( MessageDirection (..),
    MessageKind (..),
    Method,
    TNotificationMessage (..),
    TRequestMessage (..),
  )
import Language.LSP.Protocol.Types (ClientCapabilities)
import Language.LSP.Server (Handler, Handlers, mapHandlers)
import Log (sDebug)
import Monad (ServerM)

mkHandlers :: ClientCapabilities -> Handlers ServerM
mkHandlers _ =
  mapHandlers
    logRequest
    logNotification
    ( mconcat
        [ Handlers.Custom.RunCN.handler,
          Handlers.Initialized.handler,
          Handlers.TextDocument.DidChange.handler,
          Handlers.TextDocument.DidClose.handler,
          Handlers.TextDocument.DidOpen.handler,
          Handlers.TextDocument.DidSave.handler,
          Handlers.Workspace.DidChangeConfiguration.handler
        ]
    )

logNotification ::
  forall (m :: Method 'ClientToServer 'Notification).
  Handler ServerM m ->
  Handler ServerM m
logNotification handler = \notif ->
  do
    sDebug $ "Handling notification " <> Text.toStrict (Aeson.encodeToLazyText notif._method)
    handler notif

logRequest ::
  forall (m :: Method 'ClientToServer 'Request).
  Handler ServerM m ->
  Handler ServerM m
logRequest handler = \request response ->
  do
    sDebug $ "Handling request " <> Text.toStrict (Aeson.encodeToLazyText request._method)
    handler request response
