{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module EU4.Policies where

import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import System.FilePath (FilePath)

import Abstract
import Doc
import EU4.Common
import EU4.Feature
import SettingsTypes

data Policy = Policy {
        policyName :: Text
    ,   policyPath :: FilePath
    }
newPolicy = Policy {
        policyName = error "policyName undefined"
    ,   policyPath = error "policyPath undefined"
    }

instance Feature EU4 Policy where
    emptyFeature = newPolicy
    featureDirectory _ = "common/policies"
    featurePath = policyPath
    readFeatures = readPolicy
    loadFeature = loadPolicy
    getFeatures _ eu4 = policies eu4
    ppFeature = ppPolicy

readPolicy :: MonadError Text m => GenericStatement -> PPT extra m [Either Text (Maybe Policy)]
readPolicy _ = throwError "not implemented"

loadPolicy :: Policy -> EU4 -> EU4
loadPolicy mn tab@EU4 { policies = mns }
    = tab { policies = HM.insert (policyName mn) mn mns }

ppPolicy :: MonadError Text m => Policy -> PPT EU4 m Doc
ppPolicy _ = throwError "not implemented"
