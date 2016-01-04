{-# LANGUAGE OverloadedStrings #-}
module EU4.Missions where

import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import SettingsTypes

processMission :: GenericStatement -> PP extra (Either Text Doc)
processMission _ = return $ Left "not implemented"
