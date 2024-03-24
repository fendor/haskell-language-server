{-# LANGUAGE CPP #-}
module Development.IDE.Monitoring.EKG(Log, monitoring) where

import           Development.IDE.Types.Monitoring (Monitoring (..))
import           Ide.Logger

#ifdef MONITORING_EKG
import           Control.Concurrent               (killThread)
import           Control.Concurrent.Async         (async, waitCatch)
import           Control.Monad                    (forM_)
import           Data.Text                        (pack)
import           Ide.Logger                       (Pretty(..), logWith)
import qualified System.Metrics                   as Monitoring
import qualified System.Remote.Monitoring.Wai     as Monitoring
#endif

data Log
    = LogServerStartup Int
    | LogServerStop
    | LogServerBindError Int String
    deriving Show

instance Pretty Log where
    pretty = \case
        LogServerStartup port -> "Started monitoring server on port" <+> viaShow port
        LogServerStop -> "Stopping monitoring server"
        LogServerBindError port e -> "Unable to bind monitoring server on port" <+> viaShow port <> ":" <+> pretty e

#ifdef MONITORING_EKG
-- | Monitoring using EKG
monitoring :: Recorder (WithPriority Log) -> Int -> IO Monitoring
monitoring recorder port = do
    store <- Monitoring.newStore
    Monitoring.registerGcMetrics store
    let registerCounter name read = Monitoring.registerCounter name read store
        registerGauge name read = Monitoring.registerGauge name read store
        start = do
            server <- do
                let startServer = Monitoring.forkServerWith store "localhost" port
                -- this can fail if the port is busy, throwing an async exception back to us
                -- to handle that, wrap the server thread in an async
                mb_server <- async startServer >>= waitCatch
                case mb_server of
                    Right s -> do
                        logWith recorder Info $ LogServerStartup port
                        return $ Just s
                    Left e -> do
                        logWith recorder Info  $ LogServerBindError port (show e)
                        return Nothing
            return $ forM_ server $ \s -> do
                logWith recorder Info LogServerStop
                killThread $ Monitoring.serverThreadId s
    return $ Monitoring {..}

#else

monitoring :: Recorder (WithPriority Log) -> Int -> IO Monitoring
monitoring _ _ = mempty

#endif
