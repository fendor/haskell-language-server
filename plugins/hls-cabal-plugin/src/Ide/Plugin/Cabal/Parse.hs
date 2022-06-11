module Ide.Plugin.Cabal.Parse
( parseCabalFile
  -- * Re-exports
, FilePath
, NonEmpty(..)
, PWarning(..)
, Version(..)
, PError(..)
, Position(..)
, GenericPackageDescription(..)
) where

import Distribution.PackageDescription.Parsec
    ( parseGenericPackageDescription )
import Distribution.Types.GenericPackageDescription
    ( GenericPackageDescription(..) )
import qualified Data.ByteString as BS
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.Parsec.Position (Position(..))
import Distribution.Fields (PWarning(..), PError(..))
import qualified System.Directory as Dir
import Control.Monad (unless)
import qualified System.Exit as Exit
import Distribution.Types.Version (Version(..))
import Data.List.NonEmpty (NonEmpty(..))


parseCabalFile
    :: FilePath
    -> IO ([PWarning], Either (Maybe Version, NonEmpty PError) GenericPackageDescription)
parseCabalFile =
    readGenericPackageDescription'
  where
    readGenericPackageDescription' = readAndParseFile' parseGenericPackageDescription
    readAndParseFile' parser fpath = do
        exists <- Dir.doesFileExist fpath
        unless exists $
            Exit.die $
                "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
        bs <- BS.readFile fpath
        pure $ runParseResult (parser bs)
