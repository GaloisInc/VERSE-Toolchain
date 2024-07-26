module Log
  ( cInfoW,
    cDebug,
    cInfo,
    cWarn,
    cError,
    sDebug,
    sInfo,
    sWarn,
    sError,
  )
where

import Colog.Core.Action ((<&))
import Colog.Core.Severity (WithSeverity (..))
import Colog.Core.Severity qualified as Colog
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.LSP.Logging (logToLogMessage, logToShowMessage)
import Monad (ServerEnv (seLogHdl), ServerM)
import System.IO (hFlush, hPutStrLn)

cInfoW :: Text -> ServerM ()
cInfoW = clientWindow Colog.Info

-- | Send the client a debug-level message
cDebug :: Text -> ServerM ()
cDebug = clientLog Colog.Debug

-- | Send the client an info-level message
cInfo :: Text -> ServerM ()
cInfo = clientLog Colog.Info

-- | Send the client a warning-level message
cWarn :: Text -> ServerM ()
cWarn = clientLog Colog.Warning

-- | Send the client an error-level message
cError :: Text -> ServerM ()
cError = clientLog Colog.Error

-- | Log a debug-level message within the server
sDebug :: Text -> ServerM ()
sDebug = serverLog Colog.Debug

-- | Log an info-level message within the server
sInfo :: Text -> ServerM ()
sInfo = serverLog Colog.Info

-- | Log a warning-level message within the server
sWarn :: Text -> ServerM ()
sWarn = serverLog Colog.Warning

-- | Log an error-level message within the server
sError :: Text -> ServerM ()
sError = serverLog Colog.Error

clientWindow :: Colog.Severity -> Text -> ServerM ()
clientWindow severity msg = logToShowMessage <& msg `WithSeverity` severity

-- | Send the client a log message with the specified severity
clientLog :: Colog.Severity -> Text -> ServerM ()
clientLog severity msg = logToLogMessage <& msg `WithSeverity` severity

-- | Log a message within the server at the specified severity
serverLog :: Colog.Severity -> Text -> ServerM ()
serverLog severity msg =
  do
    logHdl <- asks seLogHdl
    let str = formatMessage (msg `WithSeverity` severity)
    liftIO (hPutStrLn logHdl str >> hFlush logHdl)

-- | Pretty-print a log message
formatMessage :: WithSeverity Text -> String
formatMessage ws = unwords [severity, msg]
  where
    severity =
      case getSeverity ws of
        Colog.Debug -> "[DEBUG]"
        Colog.Info -> "[INFO] "
        Colog.Warning -> "[WARN] "
        Colog.Error -> "[ERROR]"

    msg = Text.unpack (getMsg ws)
