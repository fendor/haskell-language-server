module Env where

import           Control.Monad.IO.Class
import           Control.Monad
import           System.FilePath
import           System.Info                              ( os )
import           System.Process
import           Data.Maybe                               ( isJust
                                                          , mapMaybe
                                                          )
import           System.Directory                         ( findExecutable
                                                          , findExecutables
                                                          , listDirectory
                                                          )
import           Data.Function                            ( (&)
                                                          , on
                                                          )
import           Data.List                                ( sort
                                                          , sortBy
                                                          , isInfixOf
                                                          , nubBy
                                                          )
import           Data.Ord                                 ( comparing )
import           Control.Monad.Extra                      ( mapMaybeM )

import qualified Data.Text                     as T

import           Version
import           Print
import           Utils


type GhcPath = String

existsExecutable :: String -> IO Bool
existsExecutable executable = isJust <$> findExecutable executable


-- | Check if the current system is windows
isWindowsSystem :: Bool
isWindowsSystem = os `elem` ["mingw32", "win32"]

findInstalledGhcs :: IO [(VersionNumber, GhcPath)]
findInstalledGhcs = do
  hlsVersions <- getHlsVersions
  knownGhcs <- mapMaybeM
    (\version -> getGhcPathOf version >>= \case
        Nothing -> return Nothing
        Just p  -> return $ Just (version, p)
    )
    (reverse hlsVersions)
  -- filter out not supported ghc versions
  availableGhcs <- filter ((`elem` hlsVersions) . fst) <$> getGhcPaths
  return
    -- sort by version to make it coherent with getHlsVersions
    $ sortBy (comparing fst)
    -- nub by version. knownGhcs takes precedence.
    $ nubBy ((==) `on` fst)
    -- filter out stack provided GHCs (assuming that stack programs path is the default one in linux)
    $ filter (not . isInfixOf ".stack" . snd) (knownGhcs ++ availableGhcs)

showInstalledGhcs :: [(VersionNumber, GhcPath)] -> IO ()
showInstalledGhcs ghcPaths = do
  let msg = "Found the following GHC paths: \n"
              ++ unlines
                  (map (\(version, path) -> "ghc-" ++ version ++ ": " ++ path)
                    ghcPaths
                  )
  printInStars msg

checkInstalledGhcs :: [(VersionNumber, GhcPath)] -> IO ()
checkInstalledGhcs ghcPaths = when (null ghcPaths) $ do
  let msg = "No ghc installations found in $PATH. \n"
             ++ "The script requires at least one ghc in $PATH \n"
             ++ "  to be able to build haskell-language-server.\n"
  printInStars msg
  error msg

-- | Get the path to a GHC that has the version specified by `VersionNumber`
-- If no such GHC can be found, Nothing is returned.
-- First, it is checked whether there is a GHC with the name `ghc-$VersionNumber`.
-- If this yields no result, it is checked, whether the numeric-version of the `ghc`
-- command fits to the desired version.
getGhcPathOf :: VersionNumber -> IO (Maybe GhcPath)
getGhcPathOf ghcVersion =
  liftIO $ findExecutable ("ghc-" ++ ghcVersion <.> exe) >>= \case
    Nothing -> lookup ghcVersion <$> getGhcPaths
    path -> return path

-- | Get a list of GHCs that are available in $PATH
getGhcPaths :: IO [(VersionNumber, GhcPath)]
getGhcPaths = do
  paths <- findExecutables "ghc"
  forM paths $ \path -> do
    version <- readProcess path ["--numeric-version"] ""
    return (trim version, path)

-- | No suitable ghc version has been found. Show a message.
ghcVersionNotFoundFailMsg :: VersionNumber -> String
ghcVersionNotFoundFailMsg versionNumber =
  "No GHC with version "
    <> versionNumber
    <> " has been found.\n"
    <> "Either install a fitting GHC, use the stack targets or modify the PATH variable accordingly."


-- | Defines all different hls versions that are buildable.
--
-- The current directory is scanned for `stack-*.yaml` files.
getHlsVersions ::IO [VersionNumber]
getHlsVersions = do
  let stackYamlPrefix = T.pack "stack-"
  let stackYamlSuffix = T.pack ".yaml"
  files <- listDirectory "."
  let hlsVersions =
        files
          & map T.pack
          & mapMaybe
              (T.stripPrefix stackYamlPrefix >=> T.stripSuffix stackYamlSuffix)
          & map T.unpack
        -- the following line excludes `8.6.3`, `8.8.1` and `8.8.2` on windows systems
          & filter (\p -> not (isWindowsSystem && p `elem` ["8.6.3", "8.8.1", "8.8.2"]))
          & sort
  return hlsVersions


-- | Most recent version of hls.
-- Shown in the more concise help message.
mostRecentHlsVersion :: IO VersionNumber
mostRecentHlsVersion = last <$> getHlsVersions
