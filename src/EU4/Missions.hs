{-# LANGUAGE OverloadedStrings #-}
module EU4.Missions where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract
import Doc
import SettingsTypes

processMission :: GenericStatement -> PP extra (Either Text Doc)
processMission _ = return $ Left "not implemented"
