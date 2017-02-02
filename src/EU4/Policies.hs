{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module EU4.Policies where

import Control.Monad.Except (MonadError (..))

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath (FilePath)

import Text.PrettyPrint.Leijen.Text (Doc)

import Abstract -- everything
import SettingsTypes (PPT)

processPolicy :: MonadError Text m => GenericStatement -> PPT g m [Either Text (FilePath, Doc)]
processPolicy _ = throwError "not implemented"
