{-# LANGUAGE OverloadedStrings #-}
module EU4.IO (
        readEU4Scripts
    ,   module FileIO
    ) where

import Control.Monad

import System.Directory
import System.FilePath
import System.IO

import Abstract
import FileIO
import SettingsTypes

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
        files <- filterM (doesFileExist . buildPath settings . (sourceSubdir </>))
                    =<< getDirectoryContents sourceDir
        forM files $ \filename -> do
            let target = sourceSubdir </> filename
            content <- readScript settings target
            when (null content) $
                hPutStrLn stderr $ "Warning: " ++ target ++ " contains no scripts - failed parse? Expected feature type " ++ category
            return (target, content)

