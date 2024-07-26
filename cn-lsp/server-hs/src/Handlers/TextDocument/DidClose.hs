{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Handlers.TextDocument.DidClose where

import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy qualified as Text
import Language.LSP.Protocol.Message (Method (..), SMethod (..), TNotificationMessage (..))
import Language.LSP.Protocol.Types (DidCloseTextDocumentParams)
import Language.LSP.Server (Handlers, notificationHandler)
import Log (sDebug)
import Monad (ServerM)

handler :: Handlers ServerM
handler = notificationHandler SMethod_TextDocumentDidClose doDidClose

-- | A no-op to prevent noisy error messages appearing in the client every time
-- a document is closed
doDidClose ::
  TNotificationMessage 'Method_TextDocumentDidClose ->
  ServerM ()
doDidClose notif =
  do
    sDebug $ Text.toStrict (Aeson.encodeToLazyText params)
  where
    params :: DidCloseTextDocumentParams
    params = notif._params
