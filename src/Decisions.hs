{-# LANGUAGE OverloadedStrings #-}
module Decisions where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract

processDecisionGroup :: GenericStatement -> Either Text Text
processDecisionGroup (Statement (GenericLhs head) _) = Left "not implemented"
processDecisionGroup _ = Left "invalid statement LHS"

processDecision :: GenericStatement -> Either Text Text
processDecision _ = Left "not implemented"
