{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module EU4.Policies where

import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath (FilePath)

import Abstract
import Doc
import SettingsTypes

processPolicy :: MonadError Text m => GenericStatement -> PPT m [Either Text (FilePath, Doc)]
processPolicy _ = throwError "not implemented"
