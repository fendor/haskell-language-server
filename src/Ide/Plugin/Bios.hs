{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Bios where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Coerce
import           Data.Semigroup
import qualified Data.Aeson as Aeson
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Development.IDE.Types.Location
import           Language.Haskell.Brittany
import           Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import           Ide.Plugin.Formatter
import           Ide.Types

import           System.FilePath
import           Data.Maybe (maybeToList)

descriptor :: PluginId -> PluginDescriptor
descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginRules = mempty
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Just codeLens
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginFormattingProvider = Nothing
  , pluginCompletionProvider = Nothing
  }


codeLens :: CodeLensProvider
codeLens lf ide plId params = pure $ Right $ J.List  [CodeLens (Range (Position 0 0) (Position 0 1)) Nothing (Just $ Aeson.String "Test")]
