{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module EU4.Missions where

import Control.Monad.Except (MonadError (..))

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath (FilePath)

import Text.PrettyPrint.Leijen.Text (Doc)

import Abstract -- everything
import SettingsTypes (PPT)

processMission :: MonadError Text m => GenericStatement -> PPT g m [Either Text (FilePath, Doc)]
processMission _ = throwError "not implemented"
