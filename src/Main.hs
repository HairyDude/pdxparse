{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Localization
import Settings

main = do
    settings <- readSettings
    l10n <- readL10n settings

    -- test
    let testcase = "flavor_rus.EVTDESC3401"
    case HM.lookup testcase l10n of
        Nothing -> TIO.putStrLn $ testcase <> " text not found!"
        Just content -> do
            TIO.putStrLn $ "Text for " <> testcase
            TIO.putStrLn content
