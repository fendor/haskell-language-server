{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Test.Hls.Server where

import           Control.Concurrent.Extra
import           Control.Concurrent.STM             (readTVarIO)
import           Control.Lens                       ((^.))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader               (asks)
import           Control.Monad.STM
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Reader         as Reader
import qualified Data.Aeson                         as A
import qualified Data.Dependent.Map                 as DMap
import           Data.Either.Extra
import qualified Data.List                          as List
import           Data.List.NonEmpty                 (NonEmpty, toList)
import qualified Data.Map                           as Map
import           Data.Maybe                         (maybeToList)
import qualified Data.Text                          as T
import qualified Data.Text.IO                       as T
import qualified Data.Text.Lazy                     as TL
import qualified Data.Text.Lazy.Encoding            as TL
import qualified Development.IDE                    as Shake
import           Development.IDE.Core.Shake
import qualified Development.IDE.Core.Shake         as Shake
import           Development.IDE.Main               hiding (Log)
import           Ide.Logger                         (Pretty (pretty))
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Capabilities
import qualified Language.LSP.Protocol.Lens         as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types        as J
import qualified Language.LSP.Server                as LSP
import           Prelude                            hiding (log)
import           System.FilePath
import           System.IO.Extra                    (withTempDir)
import           Test.Hls
import qualified Test.Hls.FileSystem                as FS
import           Test.Hls.FileSystem                (FileSystem,
                                                     FileTree)
import           Test.Hls.Types

-- --------------------------------------------------------------------------
-- Test runners
-- --------------------------------------------------------------------------

runServerTest ::
  Pretty b =>
  (Recorder (WithPriority b) -> IdePlugins IdeState) ->
  FilePath ->
  [FileTree] ->
  TestM r ->
  IO r
runServerTest plugin testDataDir fileTree act = do
  recorder <- pluginTestRecorder
  recorder2 <- pluginTestRecorder
  serverMessagesVar <- newVar (Map.empty :: Map.Map String [A.Value])
  let
    lspComms = Shake.LspServerRequests
      { sendRequest = \m params _callback -> do
          liftIO $ modifyVar_' serverMessagesVar (pure . Map.insertWith (<>) (someMethodToMethodString $ SomeMethod m) [A.toJSON params])
          pure $ IdInt 0
      }

  resultVar <- newVar Nothing

  withTempDir $ \root -> do
    serverTestSession recorder2 mempty (plugin recorder) root $ \ide' -> do
      let ide = ide'
            { Shake.shakeExtras = (Shake.shakeExtras ide')
                { Shake.lspServerComms = lspComms
                }
            }
      vfs <- readTVarIO $ Shake.vfsVar (Shake.shakeExtras ide)
      languageContextState <- LSP.defaultLanguageContextState def vfs
      let IdePlugins [pl] = plugin recorder
          lce = LSP.LanguageContextEnv
            { LSP.resHandlers = mempty
            , LSP.resParseConfig = \_ _ -> Left ""
            , LSP.resOnConfigChange = \_ -> pure ()
            , LSP.resConfigSection = "haskell"
            , LSP.resSendMessage = \_ -> pure ()
            , LSP.resState = languageContextState
            , LSP.resClientCapabilities = fullCaps
            , LSP.resRootPath = Just root
            }
      LSP.runLspT lce $ do
        fileSystem <- liftIO $ FS.materialise root fileTree testDataDir
        let d = TestData
              { shakeState = ide
              , pluginUnderTest = pl
              , fileSystem = fileSystem
              , workingDirectory = root
              , commandServerMessages = serverMessagesVar
              }
        a <- Reader.runReaderT (runTestT $ act) d
        liftIO $ writeVar' resultVar (Just a)

    readVar resultVar >>= \case
      Nothing -> fail "Program invariant broken: tests must write their result to 'resultVar'"
      Just a -> pure a

goldenHaskellFile :: TestName -> FilePath -> FilePath -> FilePath -> FilePath -> (FilePath -> IO T.Text) -> TestTree
goldenHaskellFile title testDir path desc ext act =
  goldenGitDiff title (testDir </> path <.> desc <.> ext)
  $ TL.encodeUtf8 . TL.fromStrict
  <$> act (testDir </> path <.> ext)

-- --------------------------------------------------------------------------
-- TestM definitions
-- --------------------------------------------------------------------------

type TestM a = TestT TestData (LSP.LspM Config) a

data TestData = TestData
  { shakeState            :: IdeState
  , workingDirectory      :: FilePath
  , fileSystem            :: FileSystem
  , pluginUnderTest       :: PluginDescriptor IdeState
  , commandServerMessages :: Var (Map.Map String [A.Value])
  }

throwError :: String -> TestM a
throwError msg = liftIO $ fail msg

-- --------------------------------------------------------------------------
-- FileSystem helpers
-- --------------------------------------------------------------------------

toNfp :: FilePath -> TestM NormalizedFilePath
toNfp fp = do
  fs <- asks fileSystem
  pure $ FS.toNfp fs fp

readFileSystem :: FilePath -> TestM T.Text
readFileSystem fp = do
  fs <- asks fileSystem
  liftIO $ FS.readFileFS fs fp

writeFileSystem :: FilePath -> FS.Content -> TestM ()
writeFileSystem fp content = do
  fs <- asks fileSystem
  liftIO $ FS.writeFileFS fs fp content
  ide <- asks shakeState
  nfp <- toNfp fp
  liftIO $ join $ atomically $ recordDirtyKeys (shakeExtras ide) GetModificationTime [nfp]
  void $ liftIO $ Shake.restartShakeSession (Shake.shakeExtras ide) VFSUnmodified (fp ++ " (modified)") []

modifyFileSystem :: FilePath -> (T.Text -> T.Text) -> TestM T.Text
modifyFileSystem fp modifyF = do
  content <- readFileSystem fp
  let newContent = modifyF content
  writeFileSystem fp (FS.text newContent)
  ide <- asks shakeState
  nfp <- toNfp fp
  liftIO $  join $ atomically $ recordDirtyKeys (shakeExtras ide) GetModificationTime [nfp]
  void $ liftIO $ Shake.restartShakeSession (Shake.shakeExtras ide) VFSUnmodified (fp ++ " (modified)") []
  pure newContent

-- --------------------------------------------------------------------------
-- Rule helpers
-- --------------------------------------------------------------------------

parseModuleWithComments :: NormalizedFilePath -> TestM ()
parseModuleWithComments target =
  void $ requireRule "parseWithComments" target Shake.GetParsedModuleWithComments

typecheckModule :: NormalizedFilePath -> TestM ()
typecheckModule target =
  void $ requireRule "typecheck" target Shake.TypeCheck

modSummaryWithoutTimestamps :: NormalizedFilePath -> TestM ()
modSummaryWithoutTimestamps target =
  void $ requireRule "modSumWithoutTimeStamps" target Shake.GetModSummaryWithoutTimestamps

requireRule :: IdeRule k v => String -> NormalizedFilePath -> k -> TestM v
requireRule label target rule = do
  ide <- asks shakeState
  res <- liftIO $ Shake.runAction label ide $ do
      Shake.use rule target
  case res of
    Nothing ->
      throwError $
        "Rule "
        <> show rule <> " failed for "
        <> show (fromNormalizedFilePath target)
    Just a -> pure a

-- --------------------------------------------------------------------------
-- Handler and Command runners
-- --------------------------------------------------------------------------

runHandler ::
  PluginRequestMethod m =>
  SMethod m ->
  MessageParams m ->
  TestM (NonEmpty (Either PluginError (MessageResult m)))
runHandler m params = do
  ide <- asks shakeState
  pl <- asks pluginUnderTest
  let PluginHandlers handlers = pluginHandlers pl

  case DMap.lookup (IdeMethod m) handlers of
    Nothing -> liftIO $ fail $ "No Handler found for method: " <> show m
    Just (PluginHandler provider) -> lift $ provider (pluginId pl) ide params

runHandler_ :: (MessageResult m ~ ([r] |? k), PluginRequestMethod m) =>
  SMethod m ->
  MessageParams m ->
  TestT TestData (LSP.LspM Config) [r]
runHandler_ m params = do
  results <- runHandler m params
  let ignoreErrors = concat . fmap (fromLeft' . toEither . fromRight')
  pure $ ignoreErrors $ toList results

runCommand :: J.Command -> TestM (A.Value |? Null)
runCommand cmd = runCommandById (cmd^.L.command) (cmd^.L.arguments)

runCommandById :: T.Text -> Maybe [A.Value] -> TestM (A.Value |? J.Null)
runCommandById cmdText params = do
  ide <- asks shakeState
  pl <- asks pluginUnderTest
  let cmds = pluginCommands pl

  let cmdId = CommandId (last (T.splitOn ":" cmdText))
  case List.find ((== cmdId) . commandId) cmds of
    Nothing -> error $ "No such command found. Known ids: " <> show (map commandId cmds) <> ", got: " <> show cmdId
    Just (PluginCommand _ _ f) -> do
      case params of
        Nothing -> error "No parameters given"
        Just vals -> case traverse A.fromJSON vals of
          A.Error e -> error e
          A.Success [param] ->
            lift (runExceptT $ f ide param) >>= \case
              Left err -> error $ show $ pretty err
              Right m  -> pure m
          A.Success ps ->
            error $ "Unexpected amount of parameters. Expected: 1, but got: " <> show (length ps)

codeLensesFor :: NormalizedFilePath -> TestM [CodeLens]
codeLensesFor target = runHandler_ SMethod_TextDocumentCodeLens $ J.CodeLensParams
  { J._workDoneToken = Nothing
  , J._partialResultToken = Nothing
  , J._textDocument = J.TextDocumentIdentifier $ fromNormalizedUri $ normalizedFilePathToUri $ target
  }

executeCodeLensCmd :: CodeLens -> TestM ()
executeCodeLensCmd l = case l^.L.command of
  Nothing  -> pure ()
  Just cmd -> void $ runCommand cmd

-- --------------------------------------------------------------------------
-- Various util functions
-- --------------------------------------------------------------------------

applyServerRequestTextEdits :: TestM ()
applyServerRequestTextEdits = do
  serverMsgsVar <- asks commandServerMessages
  serverMsgs <- liftIO $ readVar serverMsgsVar

  let s = someMethodToMethodString $ SomeMethod SMethod_WorkspaceApplyEdit

  case Map.lookup s serverMsgs of
    -- not text edits,
    Nothing -> pure ()
    Just vals -> case traverse A.fromJSON vals of
      A.Error e -> error e
      A.Success (messages :: [ApplyWorkspaceEditParams]) -> forM_ (reverse messages) $ \msg -> do
        let txtEdit = msg ^. L.edit . L.changes
        forM_ (concatMap Map.toList $ maybeToList txtEdit) $ \(uri, edit) -> do
          fp <- case uriToFilePath uri of
            Nothing -> liftIO $ fail $ "Invalid filepath: " <> show uri
            Just fp -> pure fp
          contents <- liftIO $ T.readFile fp
          let newContents = foldr applyTextEdit contents (reverse edit)
          liftIO $ T.writeFile fp newContents

          ide <- asks shakeState
          nfp <- toNfp fp
          liftIO $ join $ atomically $ recordDirtyKeys (shakeExtras ide) GetModificationTime [nfp]
          void $ liftIO $ Shake.restartShakeSession (Shake.shakeExtras ide) VFSUnmodified (fp ++ " (modified)") []

          pure ()
        pure ()

  liftIO $ writeVar' serverMsgsVar (Map.delete s serverMsgs)

  pure ()
