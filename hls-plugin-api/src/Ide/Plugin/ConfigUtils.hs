{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RankNTypes #-}

module Ide.Plugin.ConfigUtils where

import           Control.Lens                  (at, (&), (?~))
import qualified Data.Aeson                    as A
import           Data.Aeson.Lens               (_Object)
import qualified Data.Aeson.Types              as A
import           Data.Default
import qualified Data.Dependent.Map            as DMap
import qualified Data.Dependent.Sum            as DSum
import           Data.List.Extra               (nubOrd)
import           Data.String                   (IsString (fromString))
import qualified Data.Text                     as T
import           Ide.Plugin.Config
import           Ide.Plugin.Properties         (toDefaultJSON,
                                                toVSCodeExtensionSchema, Properties, PropertyKey)
import           Ide.Types
import           Language.LSP.Protocol.Message
import qualified Data.Aeson.Key as A

-- | Generates a default 'Config', but retains only effective items.
--
-- For each plugin, we automatically generate config items if they provide handlers
-- for code actions, etc...
-- Naturally, we also generate plugin specific configuration.
--
-- If a plugin is single purpose, e.g., only has a single method handler, we
-- omit the config, as it is sufficiently covered by "globalOn".
pluginsToDefaultConfig :: IdePlugins a -> A.Value
pluginsToDefaultConfig IdePlugins {..} =
  -- Use '_Object' and 'at' to get at the "plugin" key
  -- and actually set it.
  A.toJSON defaultConfig & _Object . at "plugin" ?~ elems
  where
    defaultConfig@Config {} = def
    elems = A.object $ mconcat $ singlePlugin <$> ipMap
    -- Splice genericDefaultConfig and dedicatedDefaultConfig
    -- Example:
    --
    -- {
    --  "plugin-id": {
    --    "globalOn": true,
    --    "codeActionsOn": true,
    --    "codeLensOn": true,
    --    "config": {
    --      "property1": "foo"
    --     }
    --   }
    -- }
    singlePlugin pd =
      let
        PluginId pId = pluginId pd
        x = singlePluginConfig A.fromText (const A.Bool) toDefaultJSON pd
      in
        [fromString (T.unpack pId) A..= A.object x | not $ null x]

-- | Generates json schema used in haskell vscode extension
-- Similar to 'pluginsToDefaultConfig' but simpler, since schema has a flatten structure
pluginsToVSCodeExtensionSchema :: IdePlugins a -> A.Value
pluginsToVSCodeExtensionSchema IdePlugins {..} = A.object $ mconcat $ singlePlugin <$> ipMap
  where
    singlePlugin pd =
      let
        (PluginId plId) = pluginId pd
      in
        singlePluginConfig (toKey' plId) (schemaEntry plId) (toVSCodeExtensionSchema (withIdPrefix plId "config.")) pd

    schemaEntry pId desc defaultVal =
      A.object
        [ "scope" A..= A.String "resource",
          "type" A..= A.String "boolean",
          "default" A..= A.Bool defaultVal,
          "description" A..= A.String ("Enables " <> pId <> " " <> desc)
        ]
    withIdPrefix pId x = "haskell.plugin." <> pId <> "." <> x
    toKey' pId = fromString . T.unpack . withIdPrefix pId

-- | Helper function to generate a '[A.Pair]' encoding of a singe plugin configuration.
singlePluginConfig ::
  (T.Text -> A.Key) ->
  -- ^ How to modify the key in the 'A.Pair' output.
  -- Called with the name of the key.
  (T.Text -> Bool -> A.Value) ->
  -- ^ How to create the Value in 'A.Pair'.
  -- Called with a description of the value and the default value
  -- it should have.
  (forall (r :: [PropertyKey]) . Properties r -> [A.Pair]) ->
  -- ^ Specify how custom config is serialised.
  PluginDescriptor ideState ->
  -- ^ PluginDescriptor for the plugin to generate the config for.
  [A.Pair]
singlePluginConfig toKey valueSchemaDesc customConfigSchema PluginDescriptor {pluginConfigDescriptor = ConfigDescriptor {..}, ..} =
  genericSchema <> dedicatedSchema
  where
    (PluginHandlers (DMap.toList -> handlers)) = pluginHandlers
    customConfigToDedicatedSchema (CustomConfig p) = customConfigSchema p
    genericSchema =
      let x =
            [toKey "diagnosticsOn" A..= valueSchemaDesc "diagnostics" True | configHasDiagnostics]
              <> nubOrd (mconcat (handlersToGenericSchema configInitialGenericConfig <$> handlers))
        in case x of
            -- If the plugin has only one capability, we produce globalOn instead of the specific one;
            -- otherwise we don't produce globalOn at all
            [_] -> [toKey "globalOn" A..= valueSchemaDesc "plugin" (plcGlobalOn configInitialGenericConfig)]
            _   -> x
    dedicatedSchema = customConfigToDedicatedSchema configCustomConfig
    handlersToGenericSchema PluginConfig{..} (IdeMethod m DSum.:=> _) = case m of
      SMethod_TextDocumentCodeAction           -> [toKey "codeActionsOn" A..= valueSchemaDesc "code actions" plcCodeActionsOn]
      SMethod_TextDocumentCodeLens             -> [toKey "codeLensOn" A..= valueSchemaDesc "code lenses" plcCodeLensOn]
      SMethod_TextDocumentRename               -> [toKey "renameOn" A..= valueSchemaDesc "rename" plcRenameOn]
      SMethod_TextDocumentHover                -> [toKey "hoverOn" A..= valueSchemaDesc "hover" plcHoverOn]
      SMethod_TextDocumentDocumentSymbol       -> [toKey "symbolsOn" A..= valueSchemaDesc "symbols" plcSymbolsOn]
      SMethod_TextDocumentCompletion           -> [toKey "completionOn" A..= valueSchemaDesc "completions" plcCompletionOn]
      SMethod_TextDocumentPrepareCallHierarchy -> [toKey "callHierarchyOn" A..= valueSchemaDesc "call hierarchy" plcCallHierarchyOn]
      _                                        -> []
