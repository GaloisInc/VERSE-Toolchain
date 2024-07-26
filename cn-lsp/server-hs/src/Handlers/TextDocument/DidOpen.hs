{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Handlers.TextDocument.DidOpen where

import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as Text
import Language.LSP.Protocol.Message (Method (..), SMethod (..), TNotificationMessage (..))
import Language.LSP.Protocol.Types (DidOpenTextDocumentParams)
import Language.LSP.Server (Handlers, notificationHandler)
import Log (sDebug)
import Monad (ServerM)

handler :: Handlers ServerM
handler = notificationHandler SMethod_TextDocumentDidOpen doDidOpen

-- | A no-op to prevent noisy error messages appearing in the client every time
-- a document is opened
doDidOpen ::
  TNotificationMessage 'Method_TextDocumentDidOpen ->
  ServerM ()
doDidOpen notif =
  do
    sDebug $ Text.toStrict (Aeson.encodeToLazyText params)
  where
    params :: DidOpenTextDocumentParams
    params = notif._params
