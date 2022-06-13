{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ide.Plugin.Cabal where

import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Development.IDE            as D
import           GHC.Generics
import           Ide.Types
import           Language.LSP.Types
import qualified Ide.Plugin.Cabal.Parse as Parse
import qualified Ide.Plugin.Cabal.Diag as Diag
import qualified Ide.Plugin.Cabal.LicenseSuggest as LicenseSuggest
import qualified Data.List.NonEmpty as NE
import Control.DeepSeq (NFData)
import           Data.Hashable
import           Data.Typeable
import qualified Development.IDE.Core.Shake as Shake


import qualified Language.LSP.Types                    as LSP

-- import           Control.Concurrent.STM.Stats          (atomically)
import           Control.Monad.Extra
import Development.IDE.Core.Shake (restartShakeSession)
import Control.Concurrent.STM
import qualified Language.LSP.VFS as VFS
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Encoding
import Language.LSP.Server (LspM)
import Ide.Plugin.Config (Config)

data Log
  = LogText T.Text
  | LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log
    LogText log' -> pretty log'

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginRules = exampleRules recorder
  , pluginHandlers = mkPluginHandler STextDocumentCodeAction licenseSuggest
  , pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $
      \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
          logDebug (ideLogger ide) $ "Opened text document: " <> getUri _uri
          join $ atomically $ Shake.recordDirtyKeys (shakeExtras ide) GetModificationTime [file]
          restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (opened)") [Shake.mkDelayedAction "cabal parse open" Info $ void $ use Example file]
          join $ Shake.shakeEnqueue (shakeExtras ide) $ Shake.mkDelayedAction "cabal parse modified" Info $ void $ use Example file
          -- runAction "CabalParse" ide $ void $ use Example file

  , mkPluginNotificationHandler LSP.STextDocumentDidChange $
      \ide vfs _ (DidChangeTextDocumentParams identifier@VersionedTextDocumentIdentifier{_uri} changes) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        logDebug (ideLogger ide) $ "Modified text document: " <> getUri _uri
        logDebug (ideLogger ide) $ "VFS State: " <> T.pack (show vfs)
        join $ atomically $ Shake.recordDirtyKeys (shakeExtras ide) GetModificationTime [file]
        restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (modified)") [Shake.mkDelayedAction "cabal parse modified" Info $ void $ use Example file]
        join $ Shake.shakeEnqueue (shakeExtras ide) $ Shake.mkDelayedAction "cabal parse modified" Info $ void $ use Example file
        -- runAction "CabalParse" ide $ void $ use Example file

  , mkPluginNotificationHandler LSP.STextDocumentDidSave $
      \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
        whenUriFile _uri $ \file -> do
          logDebug (ideLogger ide) $ "Saved text document: " <> getUri _uri
          join $ atomically $ Shake.recordDirtyKeys (shakeExtras ide) GetModificationTime [file]
          restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (saved)") [Shake.mkDelayedAction "cabal parse saved" Info $ void $ use Example file]
          join $ Shake.shakeEnqueue (shakeExtras ide) $ Shake.mkDelayedAction "cabal parse modified" Info $ void $ use Example file
          -- runAction "CabalParse" ide $ void $ use Example file

  , mkPluginNotificationHandler LSP.STextDocumentDidClose $
        \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
          whenUriFile _uri $ \file -> do
              let msg = "Closed text document: " <> getUri _uri
              logDebug (ideLogger ide) msg
              join $ atomically $ Shake.recordDirtyKeys (shakeExtras ide) GetModificationTime [file]
              restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " (closed)") [Shake.mkDelayedAction "cabal parse closed" Info $ void $ use Example file]
              join $ Shake.shakeEnqueue (shakeExtras ide) $ Shake.mkDelayedAction "cabal parse modified" Info $ void $ use Example file
              -- runAction "CabalParse" ide $ void $ use Example file
  ]

  }
  where
    whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
    whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'
    licenseSuggest
      :: IdeState
      -> PluginId
      -> CodeActionParams
      -> LspM Config (Either ResponseError (ResponseResult 'TextDocumentCodeAction))
    licenseSuggest _ _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List diags}) =
      pure $ Right $ List $ concatMap (LicenseSuggest.licenseErrorAction uri) diags

data Example = Example
    deriving (Eq, Show, Typeable, Generic)
instance Hashable Example
instance NFData   Example

type instance RuleResult Example = ()

exampleRules :: Recorder (WithPriority Log) -> Rules ()
exampleRules recorder = do
  define (cmapWithPrio LogShake recorder) $ \Example file -> do
    t <- use GetModificationTime file
    logWith recorder Debug $ LogText $ "Parse: " <> T.pack (show file) <> " " <> T.pack (show t)
    mVirtualFile <- Shake.getVirtualFile file
    contents <- case mVirtualFile of
      Just vfile -> pure $ Encoding.encodeUtf8 $ VFS.virtualFileText vfile
      Nothing -> do
        liftIO $ BS.readFile $ fromNormalizedFilePath file

    _pm <- liftIO $ Parse.parseCabalFileContents contents
    liftIO $ log' Debug $ LogText $ T.pack $ "Parsed file: " <> fromNormalizedFilePath file <> ". Result: " <> show _pm
    let diagLst = case _pm of
          (_, Left (_, pErrorNE)) ->
            NE.toList $ NE.map (Diag.errorDiag file) pErrorNE
          _ -> []
    logWith recorder Debug $ LogText $ "Diagnostics: " <> T.pack (show diagLst)
    return (diagLst, Just ())
  where
    log' = logWith recorder

commandId' :: CommandId
commandId' = "codelens.cabal.parse"
