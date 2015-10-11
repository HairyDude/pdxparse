{-# LANGUAGE OverloadedStrings #-}
module Missions where

import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Localization (L10n)

processMission :: FilePath -> L10n -> GenericStatement -> Either Text Doc
processMission _ _ _ = Left "not implemented"
