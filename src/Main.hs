{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import System.Directory

import Platform
import Settings

main :: IO ()
main = do
    -- Do platform-specific initialization
    initPlatform

    -- Read the settings file
    (settings, initState) <- readSettings

    createDirectoryIfMissing False "output"

    -- StateT Settings (ReaderT GameState IO) ()
    flip runReaderT initState $ flip evalStateT settings $ do
        -- 1) Read the game's scripts
        scripts <- join (runScriptReader <$> gets (readScripts . game))

        -- 2) Parse the game's scripts (into state)
        ($ scripts) . runScriptParser =<< gets (parseScripts . game)

        -- 3) Output the result of parsing and/or report errors
        join (runScriptWriter <$> gets (writeScripts . game))

