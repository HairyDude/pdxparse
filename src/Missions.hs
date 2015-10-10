{-# LANGUAGE OverloadedStrings #-}
module Missions where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract

processMission :: GenericStatement -> Either Text Text
processMission _ = Left "not implemented"
