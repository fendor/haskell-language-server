-- |Module for Help messages and traget descriptions
module Help where

import           Data.List                                ( intercalate )

import           Env
import           Print
import           Version
import           BuildSystem

stackCommand :: TargetDescription -> String
stackCommand target = "stack install.hs " ++ fst target ++ " [options]"

cabalCommand :: TargetDescription -> String
cabalCommand target = "cabal v2-run install.hs --project-file install/shake.project -- " ++ fst target ++ " [options]"

buildCommand :: TargetDescription -> String
buildCommand | isRunFromCabal = cabalCommand
             | otherwise = stackCommand

printUsage :: IO ()
printUsage = do
  putStrLn ""
  putStrLn "Usage:"
  putStrLnIndented (stackCommand templateTarget)
  putStrLnIndented "or"
  putStrLnIndented (cabalCommand templateTarget)

-- | short help message is printed by default
shortHelpMessage :: IO ()
shortHelpMessage = do
  hlsVersions <- getHlsVersions
  printUsage
  putStrLn ""
  putStrLn "Targets:"
  mapM_ (putStrLnIndented . showHelpItem (spaces hlsVersions)) (targets hlsVersions)
  putStrLn ""
 where
  spaces hlsVersions = space (targets hlsVersions)
  targets hlsVersions =
    [ ("help", "Show help message including all targets")
    , emptyTarget
    , buildTarget
    , buildLatestTarget
    , hlsTarget $ last hlsVersions
    , buildDataTarget
    , cabalGhcsTarget
    ]

-- | A record that specifies for each build system which versions of @haskell-language-server@ can be built.
data BuildableVersions = BuildableVersions
  { stackVersions :: [VersionNumber]
  , cabalVersions :: [VersionNumber]
  }

getDefaultBuildSystemVersions :: BuildableVersions -> [VersionNumber]
getDefaultBuildSystemVersions BuildableVersions {..}
  | isRunFromStack = stackVersions
  | isRunFromCabal = cabalVersions
  | otherwise      = error $ "unknown build system: " ++ buildSystem

helpMessage :: BuildableVersions -> IO ()
helpMessage versions@BuildableVersions {..} = do
  printUsage
  putStrLn ""
  putStrLn "Targets:"
  mapM_ (putStrLnIndented . showHelpItem spaces) targets
  putStrLn ""
  putStrLn "Options:"
  mapM_ (putStrLnIndented . showHelpItem spaces) options
  putStrLn ""
 where
  spaces = space targets
  -- All targets the shake file supports
  targets :: [(String, String)]
  targets = intercalate
    [emptyTarget]
    [ generalTargets
    , defaultTargets
    , if isRunFromCabal then [cabalGhcsTarget] else [stackDevTarget]
    , [macosIcuTarget]
    ]
  options = [ ("-s, --silent", "Don't print anything.")
            , ("-q, --quiet", "Print less (pass repeatedly for even less).")
            , ("-V, --verbose", "Print more (pass repeatedly for even more).")
            ]

  -- All targets with their respective help message.
  generalTargets = [helpTarget]

  defaultTargets = [buildTarget, buildLatestTarget, buildDataTarget]
    ++ map hlsTarget (getDefaultBuildSystemVersions versions)

-- | Empty target. Purpose is to introduce a newline between the targets
emptyTarget :: (String, String)
emptyTarget = ("", "")

templateTarget :: (String, String)
templateTarget = ("<target>", "")

hlsTarget :: String -> TargetDescription
hlsTarget version =
  ("hls-" ++ version, "Install haskell-language-server for GHC version " ++ version)

buildTarget :: TargetDescription
buildTarget = ("hls", "Install haskell-language-server with the latest available GHC and the data files")

buildLatestTarget :: TargetDescription
buildLatestTarget = ("latest", "Install haskell-language-server with the latest available GHC")

buildDataTarget :: TargetDescription
buildDataTarget =
  ("data", "Get the required data-files for `haskell-language-server` (Hoogle DB)")

-- special targets

macosIcuTarget :: TargetDescription
macosIcuTarget = ("icu-macos-fix", "Fixes icu related problems in MacOS")

helpTarget :: TargetDescription
helpTarget = ("help", "Show help message including all targets")

cabalGhcsTarget :: TargetDescription
cabalGhcsTarget =
  ( "ghcs"
  , "Show all GHC versions that can be installed via `cabal-build`."
  )

stackDevTarget :: TargetDescription
stackDevTarget = ("dev", "Install haskell-language-server with the default stack.yaml")
