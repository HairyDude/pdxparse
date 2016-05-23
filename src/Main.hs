{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Control.Exception (try)
import Control.Monad
import Control.Monad.Reader

import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import System.Directory
import System.FilePath
import System.IO

import Doc
import Platform
import Settings

main :: IO ()
main = do
    -- Do platform-specific initialization
    initPlatform

    -- Read the settings file
    settings <- readSettings

    createDirectoryIfMissing False "output"

    -- For each feature of the game:
    forM_ (features (game settings)) $ \feature -> do
        -- 1) Read the feature's scripts
        scripts <- runScriptReader (readScripts (game settings))
                                   settings
                                   feature

        -- 2) Parse the feature's scripts
        let handler = case HM.lookup feature (handlers (game settings)) of
                Just h -> h
                Nothing -> error $ "tried to process strange feature \"" ++ feature ++ "\""

            results :: PPT (Either Text) [(FilePath, [Either Text (FilePath, Doc)])]
            results = forM scripts $ \(file, script) -> do
                            result <- local (\s -> s { currentFile = Just file })
                                            (concatMapM (runHandler handler) script)
                            return (file, result)

        -- 3) Output the result of parsing and/or report errors
        case runReaderT results settings of
            Left err -> void . putStrLn $ "Failed processing " ++ feature ++ ": " ++ T.unpack err
            Right files -> forM_ files $ \(path, mesgs) -> forM_ mesgs $ \case
                Left err -> do
                    putStrLn $ "Processing " ++ path ++ " failed: " ++ T.unpack err
                    return ()
                Right (target, output) -> do
                    -- Write to file
                    let destinationFile = "output" </> target
                        destinationDir  = takeDirectory destinationFile
                    createDirectoryIfMissing True destinationDir
                    h <- openFile destinationFile WriteMode
                    result <- try $
                        displayIO h (renderPretty 0.9 80 output)
                    case result of
                        Right () -> return ()
                        Left err -> hPutStrLn stderr $
                            "Error writing " ++ show (err::IOError)
                    hClose h

