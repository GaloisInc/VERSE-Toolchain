module Main where

import GHC.IO.Encoding (setLocaleEncoding)
import Language.LSP.Server qualified as LSP
import Server (mkServer)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), utf8, withFile)

main :: IO Int
main =
  do
    -- We set this explicitly because the default encoding can sometimes/somehow
    -- end up as ASCII when this is run as a child process by a language client.
    -- This leads to errors when decoding CN output, because CN prints
    -- characters outside the ASCII range.
    setLocaleEncoding utf8
    args <- getArgs
    case args of
      [logFile] -> runServer logFile
      _ -> error ("unexpected arguments: " <> show args)

runServer :: FilePath -> IO Int
runServer logFile =
  withFile
    logFile
    WriteMode
    (\hdl -> LSP.runServer (mkServer hdl))
