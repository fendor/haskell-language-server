{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Cabal where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text as T
import           Development.IDE            as D
import           GHC.Generics
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types
import qualified Ide.Plugin.Cabal.Parse as Parse
import qualified Ide.Plugin.Cabal.Diag as Diag
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as Map
import Language.LSP.Server ( sendRequest )

newtype Log = LogText T.Text deriving Show

instance Pretty Log where
  pretty = \case
    LogText log' -> pretty log'

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeLens (codeLens recorder)
  , pluginCommands = [PluginCommand commandId' "example adding" addTodoCmd]
  }

commandId' :: CommandId
commandId' = "codelens.cabal.parse"

codeLens :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'TextDocumentCodeLens
codeLens recorder _ideState plId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = liftIO $ do
    log' Debug $ LogText "ExampleCabal.codeLens entered (ideLogger)"
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> filePath) -> do
        res <- Parse.parseCabalFile (fromNormalizedFilePath filePath)
        let a = case res of
              (_, Left (_, pErrorNE)) ->
                let diagLst = NE.map (Diag.errorDiag filePath) pErrorNE
                in List $ NE.toList $ NE.map (\(_, _, diag') -> mkCodeLens diag') diagLst
              _ -> List []
        pure $ Right a
      Nothing -> pure $ Right $ List []
  where
    log' = logWith recorder
    mkCodeLens diag' =
      let
        cmdParams = AddTodoParams uri diag'
        cmd = mkLspCommand plId commandId' title (Just [toJSON cmdParams])
        title = "Add TODO Item via Code Lens"
      in CodeLens (Diag._range diag') (Just cmd) Nothing
data AddTodoParams = AddTodoParams
  { file     :: Uri  -- ^ Uri of the file to add the pragma to
  , diag :: Diagnostic
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

addTodoCmd :: CommandFunction IdeState AddTodoParams
addTodoCmd _ide (AddTodoParams uri diag') = do
  let
    textEdits = List
      [TextEdit (Diag._range diag')
                  ("-- Parser error:" <> Diag._message diag' <> "\n")
      ]
    res = WorkspaceEdit
      (Just $ Map.singleton uri textEdits)
      Nothing
      Nothing
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing res) (\_ -> pure ())
  return $ Right Null
