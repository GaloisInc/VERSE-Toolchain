{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Handlers.TextDocument.DidSave where

import Control.Monad (when)
import Data.Text qualified as Text
import Handlers.Custom.RunCN qualified as RunCN
import Language.LSP.Protocol.Message (Method (..), SMethod (..), TNotificationMessage (..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (Handlers, getConfig, notificationHandler)
import Log (cError)
import Monad (Config (..), ServerM)

handler :: Handlers ServerM
handler = notificationHandler SMethod_TextDocumentDidSave doDidSave

doDidSave ::
  TNotificationMessage 'Method_TextDocumentDidSave ->
  ServerM ()
doDidSave notif =
  do
    Config {..} <- getConfig
    when cfgRunCNOnSave $
      case getFilePath notif of
        Left err -> cError (Text.pack err)
        Right filePath -> RunCN.doCNDiagnostics filePath

getFilePath :: TNotificationMessage 'Method_TextDocumentDidSave -> Either String FilePath
getFilePath notif =
  case LSP.uriToFilePath uri of
    Nothing -> Left $ "Couldn't resolve URI: " <> show uri
    Just p -> Right p
  where
    uri = notif._params._textDocument._uri
