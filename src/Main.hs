{-# LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace

import Control.Monad
import Data.Maybe
import Control.Monad.Reader

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import System.Directory
import System.FilePath
import System.IO

import Abstract
import Doc
import FileIO
import Settings

-- Script handlers
import EU4.Decisions
import EU4.Missions
import EU4.Events
import EU4.IdeaGroups
import EU4.Policies

-- Extra info for PP: idea group table
type Extra = HashMap Text IdeaGroup

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readScripts :: Settings Extra -> FilePath -> IO [(FilePath, GenericScript)]
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
collateBasicIdeaGroups :: FilePath -> Settings Extra -> (FilePath, GenericScript)
collateBasicIdeaGroups file settings
    = (file,
       map (\key -> Statement (GenericLhs "basic idea group") (GenericRhs key))
           (HM.keys . fromJust . info $ settings)) -- If this blows up, we can't continue anyway.

main :: IO ()
main = do
    settings <- readSettings ((Just <$>) . readIdeaGroupTable)

    createDirectoryIfMissing False "output"

    forM_ ["decisions"{-,"missions","events","policies","ideagroups"-}] $ \category -> do
        scripts <- readScripts settings category -- :: [(FilePath, GenericScript)]

        let handler :: GenericStatement -> PP Extra (Either Text Doc)
            handler = case category of
                "decisions" -> processDecisionGroup
                "missions" -> processMission
                "events" -> processEvent
                "policies" -> processPolicy
                "ideagroups" -> processIdeaGroup
                _ -> error $ "tried to process strange category \"" ++ category ++ "\""

            results :: PP Extra [(FilePath, [Either Text Doc])]
            results = mapM (\(file, script) ->
                            (,) file <$>
                                local (\s -> s { currentFile = Just file })
                                      (mapM handler script))
                -- for testing -- comment out for release
--                . filter (\(file, _) -> file `elem`
--                    ["events/RandomEvents.txt"
--                    ,"events/RandomProvinceEvents.txt"
--                    ])
                $ scripts

        forM_ (runReader results settings) $ \(path, mesgs) -> do
            forM_ mesgs $ \mesg -> do
                case mesg of
                    Left err -> do
                        putStrLn $ "Processing " ++ path ++ " failed: " ++ T.unpack err
                        return ()
                    Right output -> do
                        let destinationFile = "output" </> path
                            destinationDir  = takeDirectory destinationFile
                        createDirectoryIfMissing True destinationDir
                        h <- openFile destinationFile AppendMode
                        displayIO h (renderPretty 0.9 80 output)
                        hClose h

