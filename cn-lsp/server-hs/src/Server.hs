{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import CN (getCN)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import Data.Text qualified as Text
import Handlers (mkHandlers)
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (LanguageContextEnv, type (<~>) (Iso))
import Language.LSP.Server qualified as LSP
import Monad (Config, ServerEnv (..), ServerM, defConfig, runServerM)
import System.IO (Handle)

mkServer :: Handle -> LSP.ServerDefinition Config
mkServer logHdl = LSP.ServerDefinition {..}
  where
    defaultConfig :: Config
    defaultConfig = defConfig

    configSection :: Text
    configSection = "CN"

    parseConfig :: Config -> Value -> Either Text Config
    parseConfig _oldCfg newCfg =
      first Text.pack $
        Aeson.parseEither Aeson.parseJSON newCfg

    onConfigChange :: Config -> ServerM ()
    onConfigChange _ = pure ()

    doInitialize ::
      LanguageContextEnv Config ->
      LSP.TRequestMessage 'LSP.Method_Initialize ->
      IO (Either LSP.ResponseError ServerEnv)
    doInitialize ctxEnv _ =
      getCN >>= \case
        Just cn ->
          let env =
                ServerEnv
                  { seCtxEnv = ctxEnv,
                    seLogHdl = logHdl,
                    seCN = cn
                  }
           in pure (Right env)
        Nothing ->
          let err =
                LSP.ResponseError
                  (LSP.InR LSP.ErrorCodes_InternalError)
                  "No CN executable found on path"
                  -- This tells the client to offer the user a chance to retry
                  -- server initialization - see
                  -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeError.
                  --
                  -- I believe `retry` is the only allowed field here, and I
                  -- would expect that to be codified at the type level, rather
                  -- than allowing this to be a bare `Value` - see
                  -- https://github.com/haskell/lsp/issues/586.
                  (Just (Aeson.object ["retry" .= True]))
           in pure (Left err)

    staticHandlers :: LSP.ClientCapabilities -> LSP.Handlers ServerM
    staticHandlers = mkHandlers

    interpretHandler :: ServerEnv -> ServerM <~> IO
    interpretHandler serverEnv = Iso (runServerM serverEnv) liftIO

    options :: LSP.Options
    options = serverOptions

serverOptions :: LSP.Options
serverOptions =
  LSP.defaultOptions
    { LSP.optTextDocumentSync =
        Just
          LSP.TextDocumentSyncOptions
            { -- Don't send notifications of documents being opened or closed
              _openClose = Just False,
              -- Don't send any updates as the text of a document changes
              _change = Just LSP.TextDocumentSyncKind_None,
              _willSave = Just False,
              _willSaveWaitUntil = Just False,
              -- Do send notifications of documents being saved, but don't
              -- include the text
              _save = Just (LSP.InR (LSP.SaveOptions {_includeText = Just False}))
            }
    }
