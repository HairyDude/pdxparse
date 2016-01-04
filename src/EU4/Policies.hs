{-# LANGUAGE OverloadedStrings #-}
module EU4.Policies where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract
import Doc
import SettingsTypes

processPolicy :: GenericStatement -> PP extra (Either Text Doc)
processPolicy _ = return $ Left "not implemented"
