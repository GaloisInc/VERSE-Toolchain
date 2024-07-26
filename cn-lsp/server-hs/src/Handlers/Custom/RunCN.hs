{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Custom.RunCN where

import CN (CNError, runCN)
import CN qualified
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (asks)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Data (Proxy (Proxy))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message (Method (..), ResponseError, SMethod (..), TRequestMessage (..))
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (Handlers, publishDiagnostics, requestHandler)
import Log (cDebug, cError, cInfoW)
import Monad (ServerM, seCN)
import System.Exit (ExitCode (..))
import Util (LSPRange (lspRange))

data RunCNParams = RunCNParams
  { textDocument :: LSP.TextDocumentIdentifier,
    text :: Maybe Text
  }
  deriving (Generic)

instance Aeson.FromJSON RunCNParams

handler :: Handlers ServerM
handler = requestHandler (SMethod_CustomMethod (Proxy @"$/runCN")) doRunCN

doRunCN ::
  TRequestMessage ('Method_CustomMethod "$/runCN") ->
  (Either ResponseError Aeson.Value -> ServerM ()) ->
  ServerM ()
doRunCN request _responder =
  case requestParams >>= getFilePath of
    Left err -> cError (Text.pack err)
    Right filePath -> doCNDiagnostics filePath
  where
    requestParams :: Either String RunCNParams
    requestParams = Aeson.parseEither Aeson.parseJSON request._params

getFilePath :: RunCNParams -> Either String FilePath
getFilePath RunCNParams {..} =
  case LSP.uriToFilePath uri of
    Nothing -> Left $ "Couldn't resolve URI: " <> show uri
    Just p -> Right p
  where
    uri = textDocument._uri

-- | Run CN on the given file and publish whatever diagnostics result
doCNDiagnostics :: FilePath -> ServerM ()
doCNDiagnostics filePath =
  do
    let nUri = LSP.normalizedFilePathToUri (LSP.toNormalizedFilePath filePath)
    publishDiagnostics 0 nUri Nothing mempty
    cn <- asks seCN
    (code, out, err) <- liftIO (runCN cn filePath)
    cDebug $ "CN exit: " <> Text.pack (show code)
    cDebug $ "CN stdout: " <> if null out then "<empty>" else Text.pack out
    cDebug $ "CN stderr: " <> if null err then "<empty>" else Text.pack err
    case code of
      ExitSuccess -> cInfoW $ "No errors in " <> Text.pack filePath
      ExitFailure _ ->
        case Aeson.eitherDecodeStrictText (Text.pack err) of
          Left e ->
            cError $ Text.unlines ["Unable to interpret CN output", Text.pack e]
          Right cnError ->
            let (errNUri, diag) = mkErrorDiag cnError
             in publishDiagnostics 100 errNUri Nothing (partitionBySource [diag])

-- | Create an error diagnostic from the provided `CNError`. When sent to the
-- client, this will result in a "red squiggle" at the relevant range in the
-- relevant file (which may be different than the file on which CN was run to
-- get the error), which will show the error's message as a tooltip if the user
-- hovers over it.
mkErrorDiag :: CNError -> (LSP.NormalizedUri, LSP.Diagnostic)
mkErrorDiag cnError = (nuri, diag)
  where
    diag =
      LSP.Diagnostic
        { _range = lspRange range,
          _severity = Just LSP.DiagnosticSeverity_Error,
          _code = Nothing,
          _codeDescription = Nothing,
          _source = Just "CN",
          _message = msg,
          _tags = Nothing,
          _relatedInformation = Nothing,
          _data_ = Nothing
        }
    range = cnError.cneLoc
    msg = Text.unlines (cnError.cneShort : maybeToList cnError.cneDescription)
    nuri = LSP.toNormalizedUri (LSP.filePathToUri file)
    file =
      case range of
        CN.Region r -> r.cnrStart.cnpFile
        CN.Point p -> p.cnpFile
