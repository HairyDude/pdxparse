{-# LANGUAGE OverloadedStrings #-}
module Policies where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract

processPolicy :: GenericStatement -> Either Text Text
processPolicy _ = Left "not implemented"
