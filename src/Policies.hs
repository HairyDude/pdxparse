{-# LANGUAGE OverloadedStrings #-}
module Policies where

import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import SettingsTypes

processPolicy :: GenericStatement -> PP (Either Text Doc)
processPolicy _ = return $ Left "not implemented"
