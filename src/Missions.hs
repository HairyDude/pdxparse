{-# LANGUAGE OverloadedStrings #-}
module Missions where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract
import Localization (L10n)

processMission :: FilePath -> L10n -> GenericStatement -> Either Text Text
processMission _ _ _ = Left "not implemented"
