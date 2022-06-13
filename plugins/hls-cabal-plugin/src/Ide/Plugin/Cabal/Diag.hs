{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
module Ide.Plugin.Cabal.Diag
( errorDiag
  -- * Re-exports
, FileDiagnostic
, Diagnostic(..)
)
where

import qualified Data.Text                  as T
import Development.IDE
    ( ShowDiagnostic(ShowDiag),
      FileDiagnostic )
import Language.LSP.Types
    ( Range(Range),
      Diagnostic(..),
      NormalizedFilePath,
      DiagnosticSeverity(DsError),
      DiagnosticSource,
      Position(Position), fromNormalizedFilePath )
import qualified Ide.Plugin.Cabal.Parse as Lib
import Distribution.Fields (showPError)

errorDiag :: NormalizedFilePath -> Lib.PError -> FileDiagnostic
errorDiag fp err@(Lib.PError (Lib.Position line column) _) =
    mkDiag fp (T.pack "parsing") DsError range (T.pack $ showPError (fromNormalizedFilePath fp) err)
  where
    -- LSP is zero-based, Cabal is one-based
    line' = line-1
    col' = column-1
    range = Range
        (Position (fromIntegral line') (fromIntegral col'))
        (Position (fromIntegral $ line' + 1) 0)

mkDiag :: NormalizedFilePath
       -> DiagnosticSource
       -> DiagnosticSeverity
       -> Range
       -> T.Text
       -> FileDiagnostic
mkDiag file diagSource sev loc msg = (file, ShowDiag,)
    Diagnostic
    { _range    = loc
    , _severity = Just sev
    , _source   = Just diagSource
    , _message  = msg
    , _code     = Nothing
    , _tags     = Nothing
    , _relatedInformation = Nothing
    }
