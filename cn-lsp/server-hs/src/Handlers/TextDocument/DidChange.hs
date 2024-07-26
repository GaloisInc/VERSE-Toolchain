{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Handlers.TextDocument.DidChange where

import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as Text
import Language.LSP.Protocol.Message (Method (..), SMethod (..), TNotificationMessage (..))
import Language.LSP.Protocol.Types (DidChangeTextDocumentParams)
import Language.LSP.Server (Handlers, notificationHandler)
import Log (sDebug)
import Monad (ServerM)

handler :: Handlers ServerM
handler = notificationHandler SMethod_TextDocumentDidChange doDidChange

-- | A no-op to prevent noisy error messages appearing in the client every time
-- a document is changed. Regardless of our preferences for document change
-- notifications, per the spec, if we support the `textDocument/did{Open,Close}`
-- methods, we also must support this method - see
-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_synchronization
doDidChange ::
  TNotificationMessage 'Method_TextDocumentDidChange ->
  ServerM ()
doDidChange notif =
  do
    sDebug $ Text.toStrict (Aeson.encodeToLazyText params)
  where
    params :: DidChangeTextDocumentParams
    params = notif._params
