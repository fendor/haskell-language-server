{-# LANGUAGE CPP #-}

-- | Plugin Compat utils.
module Development.IDE.GHC.Compat.Plugins (
    Plugin(..),
    defaultPlugin,
#if __GLASGOW_HASKELL__ >= 808
    PluginWithArgs(..),
#endif
    applyPluginsParsedResultAction,
    initializePlugins,
    ) where

import           GHC
#if MIN_VERSION_ghc(9,0,0)
import           GHC.Driver.Plugins                (Plugin (..),
                                                    PluginWithArgs (..),
                                                    defaultPlugin, withPlugins)
import qualified GHC.Runtime.Loader                as Loader
#elif __GLASGOW_HASKELL__ >= 808
import qualified DynamicLoading                    as Loader
import           Plugins                           (Plugin (..),
                                                    PluginWithArgs (..),
                                                    defaultPlugin, withPlugins)
#else
import qualified DynamicLoading                    as Loader
import           Plugins                           (Plugin (..), defaultPlugin,
                                                    withPlugins)
#endif
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env    (hscSetFlags, hsc_dflags)
import           Development.IDE.GHC.Compat.Parser as Parser

applyPluginsParsedResultAction :: HscEnv -> DynFlags -> ModSummary -> Parser.ApiAnns -> ParsedSource -> IO ParsedSource
applyPluginsParsedResultAction env dflags ms hpm_annotations parsed = do
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
  fmap hpm_module $
    runHsc env $ withPlugins
#if MIN_VERSION_ghc(9,2,0)
      env
#else
      dflags
#endif
      applyPluginAction
      (mkHsParsedModule parsed [] hpm_annotations)

initializePlugins :: HscEnv -> IO HscEnv
initializePlugins env = do
#if MIN_VERSION_ghc(9,2,0)
    Loader.initializePlugins env
#else
    newDf <- Loader.initializePlugins env (hsc_dflags env)
    pure $ hscSetFlags newDf env
#endif
