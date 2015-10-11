{-# LANGUAGE OverloadedStrings #-}
module Decisions where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract
import Localization (L10n)

processDecisionGroup :: FilePath -> L10n -> GenericStatement -> Either Text Text
processDecisionGroup _ l10n (Statement (GenericLhs head) _) = Left "not implemented"
processDecisionGroup _ _ _ = Left "invalid statement LHS"

processDecision :: GenericStatement -> Either Text Text
processDecision _ = Left "not implemented"
