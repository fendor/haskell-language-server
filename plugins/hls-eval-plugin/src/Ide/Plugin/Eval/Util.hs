{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}

-- |Debug utilities
module Ide.Plugin.Eval.Util (
    timed,
    isLiterate,
    response',
    gStrictTry,
    -- logWith,
    prettyWarnings,
) where

import           Control.Exception                     (SomeException, evaluate,
                                                        fromException)
import           Control.Monad.Error.Class             (MonadError (throwError))
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.Trans.Class             (MonadTrans (lift))
import           Control.Monad.Trans.Except            (ExceptT (..),
                                                        runExceptT)
import           Data.Aeson                            (Value)
import           Data.Bifunctor                        (second)
import           Data.String                           (IsString (fromString))
import qualified Data.Text                             as T
import           Development.IDE                       (IdeState, printOutputable)
import qualified Development.IDE.Core.PluginUtils      as PluginUtils
import           Development.IDE.GHC.Compat.Outputable
import           Development.IDE.GHC.Compat.Util       (MonadCatch, bagToList,
                                                        catch)
import           GHC.Exts                              (toList)
import           GHC.Stack                             (HasCallStack, callStack,
                                                        srcLocFile,
                                                        srcLocStartCol,
                                                        srcLocStartLine)
import           Ide.Plugin.Error
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           System.FilePath                       (takeExtension)
import           System.Time.Extra                     (duration, showDuration)
import           UnliftIO.Exception                    (catchAny)
import Development.IDE.GHC.Compat.Core (Warn(..))
import qualified Development.IDE.GHC.Compat.Core              as SrcLoc (HasSrcSpan (getLoc),
                                                                         unLoc)
import qualified System.Time.Extra as Extra

timed :: MonadIO m => (t -> Extra.Seconds -> m a) -> t -> m b -> m b
timed out name op = do
    (secs, r) <- duration op
    _ <- out name secs
    return r

-- | Log using hie logger, reports source position of logging statement
-- logWith :: (HasCallStack, MonadIO m, Show a1, Show a2) => IdeState -> a1 -> a2 -> m ()
-- logWith state key val =
--     liftIO . logPriority undefined logLevel $
--         T.unwords
--             [T.pack logWithPos, asT key, asT val]
--   where
--     logWithPos =
--         let stk = toList callStack
--             pr pos = concat [srcLocFile pos, ":", show . srcLocStartLine $ pos, ":", show . srcLocStartCol $ pos]
--          in case stk of
--               []    -> ""
--               (x:_) -> pr $ snd x

--     asT :: Show a => a -> T.Text
--     asT = T.pack . show

isLiterate :: FilePath -> Bool
isLiterate x = takeExtension x `elem` [".lhs", ".lhs-boot"]

response' :: ExceptT PluginError (LspM c) WorkspaceEdit -> ExceptT PluginError (LspM c) (Value |? Null)
response' act = do
    res <-  ExceptT (runExceptT act
             `catchAny` \e -> do
                res <- showErr e
                pure . Left  . PluginInternalError $ fromString res)
    _ <- lift $ sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing res) (\_ -> pure ())
    pure $ InR Null

gStrictTry :: (MonadIO m, MonadCatch m) => m b -> m (Either String b)
gStrictTry op =
    catch
        (op >>= fmap Right . gevaluate)
        (fmap Left . showErr)

gevaluate :: MonadIO m => a -> m a
gevaluate = liftIO . evaluate

showErr :: Monad m => SomeException -> m String
showErr e =
#if MIN_VERSION_ghc(9,3,0)
  case fromException e of
    -- On GHC 9.4+, the show instance adds the error message span
    -- We don't want this for the plugin
    -- So render without the span.
    Just (SourceError msgs) -> return $ renderWithContext defaultSDocContext
                                      $ vcat
                                      $ bagToList
                                      $ fmap (vcat . unDecorated
                                                   . diagnosticMessage
#if MIN_VERSION_ghc(9,5,0)
                                                    (defaultDiagnosticOpts @GhcMessage)
#endif
                                                   . errMsgDiagnostic)
                                      $ getMessages msgs
    _ ->
#endif
      return . show $ e

#if MIN_VERSION_ghc(9,8,0)
prettyWarnings :: Messages DriverMessage -> String
prettyWarnings = printWithoutUniques . pprMessages (defaultDiagnosticOpts @DriverMessage)
#else
prettyWarnings :: [Warn] -> String
prettyWarnings = unlines . map prettyWarn

prettyWarn :: Warn -> String
prettyWarn Warn{..} =
    T.unpack (printOutputable $ SrcLoc.getLoc warnMsg) <> ": warning:\n"
    <> "    " <> SrcLoc.unLoc warnMsg
#endif
