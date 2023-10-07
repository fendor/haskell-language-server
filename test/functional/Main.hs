module Main where

import           Config
import           Format
import           FunctionalBadProject
import           Progress
import           Test.Hls

main :: IO ()
main = defaultTestRunner $ testGroup "haskell-language-server"
    [ Config.tests
    , ignoreInEnv [HostOS Windows, GhcVer GHC90, GhcVer GHC92] "Tests gets stuck in ci" $ Format.tests
    , FunctionalBadProject.tests
    , ignoreInEnv [HostOS Windows, GhcVer GHC90] "Tests gets stuck in ci" $ Progress.tests
    ]
