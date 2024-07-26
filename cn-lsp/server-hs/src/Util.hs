module Util where

import Language.LSP.Protocol.Types qualified as LSP

-- | Things that can be interpreted as an `LSP.Range`
class LSPRange a where
  lspRange :: a -> LSP.Range

instance LSPRange LSP.Range where
  lspRange = id

-- | Things that can be interpreted as an `LSP.Position`
class LSPPosition a where
  lspPosition :: a -> LSP.Position

instance LSPRange LSP.Position where
  lspRange p = LSP.Range p p

instance LSPPosition LSP.Position where
  lspPosition = id
