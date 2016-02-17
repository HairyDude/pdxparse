{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module EU4.Missions where

import Control.Monad.Except

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import System.FilePath (FilePath)

import EU4.Common
import EU4.Feature
import Doc
import SettingsTypes

data Mission = Mission {
        missionName :: Text
    ,   missionPath :: FilePath
    } deriving (Show) -- placeholder
newMission = Mission undefined undefined

instance Feature EU4 Mission where
    emptyFeature = newMission
    featureDirectory _ = "missions"
    featurePath = missionPath
    readFeatures = readMission
    loadFeature = loadMission
    getFeatures _ eu4 = missions eu4
    ppFeature = ppMission

readMission :: MonadError Text m => GenericStatement -> PPT extra m [Either Text (Maybe Mission)]
readMission _ = throwError "not implemented"

loadMission :: Mission -> EU4 -> EU4
loadMission mn tab@EU4 { missions = mns }
    = tab { missions = HM.insert (missionName mn) mn mns }

ppMission :: Monad m => Mission -> PPT EU4 m Doc
ppMission _ = return ("<p>missions not implemented</p>")
