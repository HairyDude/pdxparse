{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.FilePath

import qualified Data.Attoparsec.Text as Ap

import Abstract
import Localization
import Settings

readScript :: Settings -> FilePath -> IO GenericScript
readScript settings file = do
    let filepath = steamDir settings </> steamApps settings </> game settings </> file
    contents <- TIO.readFile filepath
    case Ap.parseOnly (skipSpace >> genericScript) contents of
        Right result -> return result
        Left error -> do
            putStrLn $ "Couldn't parse " ++ file ++ ": " ++ error
            return []

main = do
    settings <- readSettings
    l10n <- readL10n settings

    -- test
    let testcase = "events/FlavorRUS.txt"
    script <- readScript settings testcase
    putStrLn $ "Parsed contents of " ++ testcase
    putStrLn $ show script
    putStrLn ""
    putStrLn $ "Pretty-printed contents:"
    TIO.putStrLn (displayGenericScript script)
