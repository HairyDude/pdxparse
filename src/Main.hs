{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-|
Module      : Main
Description : Entry point for pdxparse executable
-}
module Main where

import Control.Monad (join)
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.State (MonadState (..), gets, evalStateT)
import Control.Monad.Trans (MonadIO (..))

import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Text (Text, unpack)

import Platform (initPlatform)
import Settings (readSettings)
import SettingsTypes ( Settings (..), Game (..), IsGame (..)
                     , readScripts, parseScripts, writeScripts
                     , hoistExceptions)

-- | Entry point for the program.
main :: IO ()
main = do
    -- Do platform-specific initialization
    initPlatform

    -- Read the settings file
    settings <- readSettings

    createDirectoryIfMissing False "output"

    -- StateT Settings (ReaderT GameState IO) ()
    case game settings of
        Game g -> runWithInitState g settings $ do
            -- 1) Read the game's scripts
            errs <- hoistExceptions readScripts
            case errs of
                Right () -> return () -- carry on
                Left e -> liftIO $ do
                    hPutStrLn stderr $ "Failed reading scripts: " ++ unpack e
                    exitFailure

            -- 2) Parse the game's scripts (into state)
            parseScripts

            -- 3) Output the result of parsing and/or report errors
            writeScripts

