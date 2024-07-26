{-# LANGUAGE OverloadedStrings #-}

module Monad where

import CN (CNExecutable)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Language.LSP.Server (LanguageContextEnv, LspM, MonadLsp (..), runLspT)
import System.IO (Handle)

data Config = Config
  { cfgRunCNOnSave :: Bool
  }

-- | This instance recognizes field names as defined in the language client's
-- `package.json`, specifically the "configuration" section of "contributes".
instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    do
      cfgRunCNOnSave <- obj .: "runOnSave"
      pure Config {..}

-- | Our default configuration
defConfig :: Config
defConfig =
  Config
    { cfgRunCNOnSave = False
    }

data ServerEnv = ServerEnv
  { seCtxEnv :: LanguageContextEnv Config,
    seLogHdl :: Handle,
    seCN :: CNExecutable
  }

newtype ServerM a = ServerM {unServerM :: ReaderT ServerEnv (LspM Config) a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader ServerEnv,
      MonadUnliftIO
    )

instance MonadLsp Config ServerM where
  getLspEnv = asks seCtxEnv

runServerM :: ServerEnv -> ServerM a -> IO a
runServerM serverEnv (ServerM rdrAction) =
  let lspAction = runReaderT rdrAction serverEnv
   in runLspT (seCtxEnv serverEnv) lspAction
