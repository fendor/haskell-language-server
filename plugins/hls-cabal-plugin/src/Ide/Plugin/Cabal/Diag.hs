{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
module Ide.Plugin.Cabal.Diag where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Functor
import qualified Data.Text                  as T
import           Data.Typeable
import           Development.IDE            as D
import           Development.IDE.Core.Shake hiding (Log)
import qualified Development.IDE.Core.Shake as Shake
import           GHC.Generics
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Ide.Plugin.Cabal.Parse as Lib

errorDiag :: NormalizedFilePath -> Lib.PError -> FileDiagnostic
errorDiag fp (Lib.PError (Lib.Position line column) errMsg) =
    mkDiag fp (T.pack "parsing") DsError range (T.pack errMsg)
  where
    range = Range
        (Position (fromIntegral line) (fromIntegral column))
        (Position (fromIntegral $ line + 1) 0)

mkDiag :: NormalizedFilePath
       -> DiagnosticSource
       -> DiagnosticSeverity
       -> Range
       -> T.Text
       -> FileDiagnostic
mkDiag file diagSource sev loc msg = (file, D.ShowDiag,)
    Diagnostic
    { _range    = loc
    , _severity = Just sev
    , _source   = Just diagSource
    , _message  = msg
    , _code     = Nothing
    , _tags     = Nothing
    , _relatedInformation = Nothing
    }
