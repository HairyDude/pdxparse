{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module EU4.Missions where

import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath (FilePath)

import Abstract
import Doc
import SettingsTypes

processMission :: MonadError Text m => GenericStatement -> PPT m [Either Text (FilePath, Doc)]
processMission _ = throwError "not implemented"
