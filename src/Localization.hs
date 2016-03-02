{-# LANGUAGE OverloadedStrings #-}
module Localization
        ( readL10n -- :: Settings -> IO L10n
        , module SettingsTypes
        ) where

import Control.Monad

import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Yaml as Y
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Ap

import System.Directory
import System.FilePath
import System.Exit
import System.IO (hPutStrLn, stderr)

import SettingsTypes

readL10n :: Settings a -> IO L10n
readL10n settings = do
    let dir = steamDir settings
              </> steamApps settings
              </> T.unpack (gameName settings)
              </> "localisation"
        usesCSV = case game settings of
            -- EU4 (and probably later games) uses Yaml localization.
            --  l_english: # or l_french, etc.
            --      KEY: "content"
            GameEU4 -> False
            -- Older games use semicolon-separated CSV.
            --  KEY;English;French;German;;Spanish;;;;;;;;x;
            GameVic2 -> True
    files <- filterM doesFileExist
                . map (dir </>)
                . (if not usesCSV then
                    filter (language settings `isInfixOf`)
                   else id)
                    =<< getDirectoryContents dir
    if usesCSV then do
        fieldNum <- case language settings of
                "english" -> return 0
                "french" -> return 1
                "german" -> return 2
                "spanish" -> return 4
                _ -> do
                    hPutStrLn stderr ("Language "
                        ++ language settings ++ " not supported")
                    exitFailure
        let csvField :: Parser Text
            csvField = Ap.takeWhile (/=';')
            -- Key and value in the chosen language
            parseLine :: Parser (Text, Text)
            parseLine = (,)
                <$> Ap.takeWhile (/= ';')
                <*> (Ap.char ';'
                 *> ((!!fieldNum) <$> csvField `Ap.sepBy` (Ap.string ";")))
        liftM HM.unions . forM files $ \file -> do
            fileContents <- T.lines <$> TIO.readFile file
            let parsedLines = mapM (Ap.parseOnly parseLine) fileContents
            case parsedLines of
                Left err -> do
                    hPutStrLn stderr ("Error reading localization file "
                        ++ file
                        ++ ": " ++ err)
                    exitFailure
                Right lines -> return $
                    -- actual type: HashMap Text Text -> Text -> Text -> HashMap Text Text
                    foldr (uncurry HM.insert) HM.empty lines
    else
        liftM HM.unions . forM files $ \file -> do
            fileContents <- B.readFile file
            let fileContentsDoctored = B8.unlines . drop 1 . B8.lines $ fileContents
                parseResult = Y.decodeEither fileContentsDoctored
            case parseResult of
                Left exc -> do
                    hPutStrLn stderr $ "Parsing localisation file " ++ file ++ " failed: " ++ show exc
                    return HM.empty
                Right contents -> return contents
