{-# LANGUAGE OverloadedStrings #-}
module Decisions where

import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Localization (L10n)

processDecisionGroup :: FilePath -> L10n -> GenericStatement -> Either Text Doc
processDecisionGroup _ l10n (Statement (GenericLhs head) _) = Left "not implemented"
processDecisionGroup _ _ _ = Left "invalid statement LHS"

processDecision :: GenericStatement -> Either Text Doc
processDecision _ = Left "not implemented"
