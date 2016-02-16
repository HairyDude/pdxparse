{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import GHC.IO.Encoding

import Debug.Trace

import Control.Exception
import Control.Monad
import Data.Maybe
import Control.Monad.Reader

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Abstract
import Doc
import FileIO
import Platform
import Settings

-- Script handlers
import EU4.Common
import EU4.Decisions
import EU4.Missions
import EU4.Events
import EU4.IdeaGroups
import EU4.Policies

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readScripts :: Settings EU4 -> FilePath -> IO [(FilePath, GenericScript)]
readScripts settings category =
    let sourceSubdir = case category of
            "policies" -> "common" </> "policies"
            "ideagroups" -> "common" </> "ideas"
            _          -> category
        sourceDir = buildPath settings sourceSubdir
    in do
        files <- filterM (doesFileExist . buildPath settings . (sourceSubdir </>)) =<< getDirectoryContents sourceDir
        forM files $ \filename ->
            let target = sourceSubdir </> filename in
            if filename == "00_basic_ideas.txt"
            -- generic ideas are already parsed into "gets info", don't do it again
            then return (collateBasicIdeaGroups target settings)
            else do
                content <- readScript settings (buildPath settings target)
                when (null content) $
                    hPutStrLn stderr $ "Warning: " ++ target ++ " contains no scripts - failed parse?"
                return (target, content)

-- Return fake info for the ideas handler to handle basic ideas.
collateBasicIdeaGroups :: FilePath -> Settings EU4 -> (FilePath, GenericScript)
collateBasicIdeaGroups file settings
    = (file,
       map (\key -> Statement (GenericLhs "basic idea group") (GenericRhs key))
           (HM.keys . ideas . info $ settings)) -- If this blows up, we can't continue anyway.

main :: IO ()
main = do
    -- Do platform-specific initialization
    initPlatform

    -- EU4 mode
    settings <- readSettings (fmap (EU4 []) <$> readIdeaGroupTable)

    createDirectoryIfMissing False "output"

    forM_ ["decisions","missions","events","policies","ideagroups"] $ \category -> do
        scripts <- readScripts settings category -- :: [(FilePath, GenericScript)]

        -- Each handler function returns one Doc, together with an output path,
        -- for each "unit".
        let handler :: GenericStatement -> PPT EU4 (Either Text) [Either Text (FilePath, Doc)]
            handler = case category of
                "decisions" -> processDecisionGroup
                "missions" -> processMission
                "events" -> processEvent
                "policies" -> processPolicy
                "ideagroups" -> processIdeaGroup
                _ -> error $ "tried to process strange category \"" ++ category ++ "\""

            results :: PPT EU4 (Either Text) [(FilePath, [Either Text (FilePath, Doc)])]
            results = mapM (\(file, script) -> do
                            result <- local (\s -> s { currentFile = Just file })
                                            (concatMapM handler script)
                            return (file, result)
                        )
                -- for testing -- comment out for release
--                . filter (\(file, _) -> file `elem`
--                    ["common/ideas/00_basic_ideas.txt"
--                    ])
                $ scripts

        case (runReaderT results settings) of
            Left err -> void . putStrLn $ "Failed processing " ++ category ++ ": " ++ T.unpack err
            Right files -> forM_ files $ \(path, mesgs) -> forM_ mesgs $ \case
                Left err -> do
                    putStrLn $ "Processing " ++ path ++ " failed: " ++ T.unpack err
                    return ()
                Right (target, output) -> do
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

