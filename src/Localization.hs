{-# LANGUAGE OverloadedStrings #-}
module Localization (
        readL10n -- :: Settings -> IO L10n
    ,   module SettingsTypes
    ) where

import Control.Monad
import Data.Monoid

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
import Yaml

readL10n :: Settings -> IO L10n
readL10n settings = do
    let dir = steamDir settings
              </> steamApps settings
              </> gameFolder settings
              </> "localisation"
        usesCSV = case game settings of
            -- EU4 (and probably later games) uses Yaml localization.
            --  l_english: # or l_french, etc.
            --      KEY: "content"
            GameEU4 {} -> False
            GameHOI4 {} -> False
            GameStellaris {} -> False
            -- Older games use semicolon-separated CSV.
            --  KEY;English;French;German;;Spanish;;;;;;;;x;
            GameVic2 {} -> True
            GameUnknown {} ->
                error "Unknown game while reading l10n"
    files <- filterM doesFileExist
                . map (dir </>)
                . (if not usesCSV then
                    filter (T.unpack (language settings) `isInfixOf`)
                   else id)
                    =<< getDirectoryContents dir
    if usesCSV then
        let csvField :: Parser Text
            csvField = Ap.takeWhile (/=';')
            langs = [(0, "l_english")
                    ,(1, "l_french")
                    ,(2, "l_german")
                    ,(4, "l_spanish")]
            -- Key and value in the chosen language
            parseLine :: Parser L10n
            parseLine = do
                tag:fields <- csvField `Ap.sepBy` (Ap.string ";")
                -- fields :: [Text]
                let addField (fieldnum, lang)
                        = HM.singleton
                            lang
                            (HM.singleton tag (LocEntry 0 (fields!!fieldnum)))
                return $ mergeLangList (map addField langs)
        in liftM mconcat . forM files $ \file -> do
            fileContents <- T.lines <$> TIO.readFile file
            let parsedLines = mapM (Ap.parseOnly parseLine) fileContents
            case parsedLines of
                Left err -> do
                    hPutStrLn stderr ("Error reading localization file "
                        ++ file
                        ++ ": " ++ err)
                    exitFailure
                Right lines -> return (mconcat lines)
    else liftM (foldl' mergeLangs HM.empty) . forM files $ \file -> do
        parseResult <- parseLocFile <$> TIO.readFile file
        case parseResult of
            Left exc -> do
                hPutStrLn stderr $ "Parsing localisation file " ++ file ++ " failed: " ++ show exc
                return HM.empty
            Right contents -> return contents
