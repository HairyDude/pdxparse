{-# LANGUAGE OverloadedStrings #-}
module EU4.Settings (
        fillSettings
    ) where

import Control.Monad (filterM, forM, when)

import qualified Data.HashMap.Strict as HM
--import Data.Text (Text)

import System.Directory
import System.FilePath

import Abstract
import FileIO
import SettingsTypes
import EU4.IdeaGroups
import EU4.Types

-- Handlers
import EU4.Decisions (processDecisionGroup)
import EU4.Missions (processMission)
import EU4.Events (processEU4Event)
import EU4.Policies (processPolicy)

fillSettings :: Settings -> IO Settings
fillSettings settings = do
    ideaGroupTable <- readIdeaGroupTable settings
    return $ settings {
        game = GameEU4 {
            gEU4 = EU4 {
                scopeStack = []
            ,   ideas = ideaGroupTable
            }
        ,   readScripts = ScriptReader readEU4Scripts
        ,   features =
                ["decisions"
                ,"missions"
                ,"events"
                ,"policies"
                ,"ideagroups"
                ]
        ,   handlers = HM.fromList
                [("decisions",  Handler processDecisionGroup)
                ,("missions",   Handler processMission)
                ,("events",     Handler processEU4Event)
                ,("policies",   Handler processPolicy)
                ,("ideagroups", Handler processIdeaGroup)
                ]
        }
    }

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readEU4Scripts :: Settings -> FilePath -> IO [(FilePath, GenericScript)]
readEU4Scripts settings category =
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
            -- generic ideas are already parsed, don't do it again
            then return (collateBasicIdeaGroups target settings)
            else do
                content <- readScript settings target
                when (null content) $
                    hPutStrLn stderr $ "Warning: " ++ target ++ " contains no scripts - failed parse? Expected feature type " ++ category
                return (target, content)

-- Return fake info for the ideas handler to handle basic ideas.
collateBasicIdeaGroups :: FilePath -> Settings -> (FilePath, GenericScript)
collateBasicIdeaGroups file settings
    = (file,
       map (\key -> Statement (GenericLhs "basic idea group") OpEq (GenericRhs key))
           (HM.keys . ideas . gEU4 . game $ settings)) -- If this blows up, we can't continue anyway.
