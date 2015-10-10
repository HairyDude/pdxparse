{-# LANGUAGE OverloadedStrings #-}
module Events where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract

processEvent :: GenericStatement -> Either Text Text
processEvent _ = Left "not implemented"
