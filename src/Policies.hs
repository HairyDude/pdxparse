{-# LANGUAGE OverloadedStrings #-}
module Policies where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract
import Localization (L10n)

processPolicy :: FilePath -> L10n -> GenericStatement -> Either Text Text
processPolicy _ _ _ = Left "not implemented"
