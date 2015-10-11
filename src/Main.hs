{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow
import Control.Exception
import Control.Monad
import Data.List
import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath
import System.IO

import qualified Data.Attoparsec.Text as Ap

import Abstract
import Localization
import Settings

-- Script handlers
import Decisions
import Missions
import Events
import Policies

buildPath :: Settings -> FilePath -> FilePath
buildPath settings path = steamDir settings </> steamApps settings </> game settings </> path

-- Read a file as Text. Unfortunately EU4 script files use several incompatible
-- encodings. Try the following encodings in order:
-- 1. UTF-8
-- 2. ISO 8859-1
-- (Decoding as 8859-1 can't fail, but I don't know if it will always be correct.)
readFileRetry :: FilePath -> IO Text
readFileRetry path = do
    raw <- B.readFile path
    -- Catching exceptions in pure code is a rather convoluted process...
    e <- try (let e = TE.decodeUtf8 raw in e `seq` return e)
    case (e::Either UnicodeException Text) of
        Right result -> do
            return result
        Left err -> do
            return $ TE.decodeLatin1 raw

readScript :: Settings -> FilePath -> IO GenericScript
readScript settings file = do
    let filepath = buildPath settings file
    contents <- readFileRetry filepath
    case Ap.parseOnly (skipSpace >> genericScript) contents of
        Right result -> return result
        Left error -> do
            putStrLn $ "Couldn't parse " ++ file ++ ": " ++ error
            return []

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readScripts :: Settings -> FilePath -> IO [(FilePath, GenericScript)]
readScripts settings category =
    let sourceSubdir = case category of
            "policies" -> "common" </> "policies"
            _          -> category
        sourceDir = buildPath settings sourceSubdir
    in do
        files <- filterM (doesFileExist . buildPath settings . (sourceSubdir </>)) =<< getDirectoryContents sourceDir
        forM files $ \filename -> do
            let target = sourceSubdir </> filename
            content <- readScript settings (buildPath settings target)
            when (null content) $
                hPutStrLn stderr $ "Warning: " ++ target ++ " contains no scripts - failed parse?"
            return (target, content)

main :: IO ()
main = do
    settings <- readSettings
    l10n <- readL10n settings

    createDirectoryIfMissing False "output"

    forM_ ["decisions","missions","events","policies"] $ \category -> do
        scripts <- readScripts settings category -- :: [(FilePath, GenericScript)]

        let handler :: FilePath -> L10n -> GenericStatement -> Either Text Text
            handler = case category of
                "decisions" -> processDecisionGroup
                "missions" -> processMission
                "events" -> processEvent
                "policies" -> processPolicy

        let results :: [(FilePath, [Either Text Text])]
            results = map (\(file, script) -> (file, map (handler file l10n) script)) scripts

        forM_ results $ \(path, mesgs) -> do
            forM_ mesgs $ \mesg -> do
                case mesg of
                    Left err -> do
                        putStrLn $ "Processing " ++ path ++ " failed: " ++ T.unpack err
                        return ()
                    Right output -> do
                        let destinationFile = "output" </> path
                            destinationDir  = takeDirectory destinationFile
                        createDirectoryIfMissing True destinationDir
                        TIO.writeFile destinationFile output

