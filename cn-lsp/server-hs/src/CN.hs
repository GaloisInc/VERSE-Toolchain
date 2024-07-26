{-# LANGUAGE OverloadedStrings #-}

module CN where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON (parseJSON), withArray, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Vector qualified as V
import Language.LSP.Protocol.Types qualified as LSP
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Util (LSPPosition (lspPosition), LSPRange (lspRange))

newtype CNExecutable = CNExecutable FilePath

-- | Look for a CN executable in one of two places
--
-- - First, in the `CN` environment variable
-- - Failing that, on the user's `PATH`
getCN :: (MonadIO m) => m (Maybe CNExecutable)
getCN = liftIO $
  do
    envCN <- lookupEnv "CN"
    case envCN of
      Just cn -> pure (Just (CNExecutable cn))
      Nothing ->
        do
          (code, out, _err) <- readProcessWithExitCode "which" ["cn"] mempty
          case (code, lines out) of
            (ExitSuccess, pathCN : _) -> pure (Just (CNExecutable pathCN))
            (ExitSuccess, _) -> error "`which` succeeded but found nothing?"
            (ExitFailure _, _) -> pure Nothing

runCN :: (MonadIO m) => CNExecutable -> FilePath -> m (ExitCode, String, String)
runCN (CNExecutable cn) filePath =
  liftIO (readProcessWithExitCode cn ["--json", filePath] mempty)

cnPath :: CNExecutable -> FilePath
cnPath (CNExecutable f) = f

data CNError = CNError
  { cneLoc :: CNLoc,
    cneShort :: Text,
    cneDescription :: Maybe Text,
    cneStateFile :: Maybe FilePath
  }

instance FromJSON CNError where
  parseJSON = withObject "CNError" $ \obj ->
    do
      loc <- obj .: "loc"
      short <- obj .: "short"
      descr <- obj .: "descr"
      state <- obj .: "state"
      pure
        CNError
          { cneLoc = loc,
            cneShort = short,
            cneDescription = descr,
            cneStateFile = state
          }

data CNLoc
  = Region CNRegion
  | Point CNPoint

instance LSPRange CNLoc where
  lspRange cnLoc =
    case cnLoc of
      Region cnRegion -> lspRange cnRegion
      Point cnPoint -> lspRange cnPoint

instance FromJSON CNLoc where
  parseJSON = withArray "loc" $ \arr ->
    case (arr V.!? 0, arr V.!? 1) of
      (Just (Aeson.String "Region"), Just reg) -> Region <$> parseJSON reg
      (Just (Aeson.String "Point"), Just pt) -> Point <$> parseJSON pt
      _ -> fail ""

data CNRegion = CNRegion
  { cnrStart :: CNPoint,
    cnrEnd :: CNPoint
  }

instance LSPRange CNRegion where
  lspRange CNRegion {..} =
    LSP.Range
      { _start = lspPosition cnrStart,
        _end = lspPosition cnrEnd
      }

instance FromJSON CNRegion where
  parseJSON = withObject "region" $ \obj ->
    do
      start <- obj .: "region_start"
      end <- obj .: "region_end"
      pure $ CNRegion {cnrStart = start, cnrEnd = end}

data CNPoint = CNPoint
  { cnpFile :: FilePath,
    cnpLine :: Int,
    cnpChar :: Int
  }

instance LSPRange CNPoint where
  lspRange cnPoint =
    LSP.Range
      { _start = lspPosition cnPoint,
        _end = lspPosition cnPoint
      }

instance LSPPosition CNPoint where
  lspPosition CNPoint {..} =
    LSP.Position
      { _line = fromIntegral cnpLine - 1,
        _character = fromIntegral cnpChar
      }

instance FromJSON CNPoint where
  parseJSON = withObject "position" $ \obj ->
    do
      file <- obj .: "file"
      line <- obj .: "line"
      char <- obj .: "char"
      pure $ CNPoint {cnpFile = file, cnpLine = line, cnpChar = char}
