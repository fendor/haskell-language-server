{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Ide.Plugin.Cabal.LicenseSuggest
( licenseErrorSuggestion
, licenseErrorAction
  -- * Re-exports
, T.Text
, Diagnostic(..)
)
where

import qualified Data.Text                  as T
import Language.LSP.Types (Diagnostic(..), TextEdit (TextEdit), WorkspaceEdit (WorkspaceEdit), CodeActionKind (CodeActionQuickFix), CodeAction (CodeAction), type (|?) (..), List (List), Range (Range), Position (Position))
import Text.Regex.TDFA
import Data.Maybe (listToMaybe)
import qualified Data.HashMap.Strict as Map

licenseErrorAction uri diag =
    case licenseErrorSuggestion diag of
        Just (original, suggestion) ->
            let
                adjustRange (Range (Position line col) rangeTo) =
                  Range (Position line (col - fromIntegral (T.length original))) rangeTo
                title = "Replace with " <> suggestion
                tedit = [TextEdit (adjustRange $ _range diag) (suggestion <> "\n")]
                edit  = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
            in [InR $ CodeAction title (Just CodeActionQuickFix) (Just $ List []) Nothing Nothing (Just edit) Nothing Nothing]
        Nothing -> []

-- | Given a diagnostic, if it represents an "Unknown SPDX license identifier"-error
-- then return the suggestion (after the "Do you mean"-text), if present.
licenseErrorSuggestion :: Diagnostic -> Maybe (T.Text, T.Text)
licenseErrorSuggestion diag =
  mSuggestion (_message diag) >>= \case
    [original, suggestion] -> Just (original, suggestion)
    _ -> Nothing
  where
    regex :: T.Text
    regex = "Unknown SPDX license identifier: '(.*)' Do you mean (.*)\\?"
    mSuggestion msg = getMatch <$> (msg :: T.Text) =~~ regex
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results
