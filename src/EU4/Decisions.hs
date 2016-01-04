{-# LANGUAGE OverloadedStrings #-}
module EU4.Decisions where

import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import SettingsTypes

processDecisionGroup :: GenericStatement -> PP extra (Either Text Doc)
processDecisionGroup (Statement (GenericLhs head) _) = return $ Left "not implemented"
processDecisionGroup _ = return $ Left "invalid statement LHS"

processDecision :: GenericStatement -> PP extra (Either Text Doc)
processDecision _ = return $ Left "not implemented"
