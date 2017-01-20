{-# LANGUAGE OverloadedStrings #-}
module Stellaris.Settings (
        fillSettings
    ,   writeStellarisScripts 
    ,   module Stellaris.Types
    ) where

import Control.Monad (join, when, forM, filterM)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (MonadState (..), modify)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import FileIO (buildPath, readScript)
import SettingsTypes ( PPT, Game (..), Settings (..), GameScripts (..), GameState (..)
                     , ScriptReader (..) -- TODO: eliminate this
                     , ScriptParser (..) -- TODO: eliminate this
                     , ScriptWriter (..) -- TODO: eliminate this
                     )
import Stellaris.Types -- everything

-- Handlers
import Stellaris.Events (parseStellarisEvents, writeStellarisEvents)

fillSettings :: Settings -> IO (Settings, GameState)
fillSettings settings = return $
    (settings {
        game = GameStellaris {
            readScripts = ScriptReader readStellarisScripts
        ,   parseScripts = ScriptParser parseStellarisScripts
        ,   writeScripts = ScriptWriter writeStellarisScripts
        ,   stdata = StellarisData HM.empty
        }
    }, StellarisState (Stellaris {
        scopeStack = []
       }) Nothing Nothing)

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readStellarisScripts :: PPT IO GameScripts
readStellarisScripts = GameScriptsStellaris <$> do
    let readStellarisScript :: String -> PPT IO (HashMap String GenericScript)
        readStellarisScript category = do
            settings <- get
            let sourceSubdir = {-case category of
                    "policies" -> "common" </> "policies"
                    "ideagroups" -> "common" </> "ideas"
                    _          ->-} category
                sourceDir = buildPath settings sourceSubdir
            files <- liftIO (filterM (doesFileExist . buildPath settings . (sourceSubdir </>))
                                     =<< getDirectoryContents sourceDir)
            results <- forM files $ \filename -> do
                let target = sourceSubdir </> filename
                content <- join (liftIO . flip readScript target <$> get)
                when (null content) $
                    liftIO $ hPutStrLn stderr $
                        "Warning: " ++ target
                            ++ " contains no scripts - failed parse? Expected feature type "
                            ++ category
                return (target, content)
            return $ foldl (flip (uncurry HM.insert)) HM.empty results

    events <- readStellarisScript "events"
    return $ StellarisScripts {
            steventScripts = events
        }

parseStellarisScripts :: Monad m => GameScripts -> PPT m ()
parseStellarisScripts (GameScriptsStellaris (StellarisScripts {
                    steventScripts = eventScripts
                })) = do
    events <- parseStellarisEvents eventScripts
    
    modify $ \s -> case game s of
        GameStellaris { stdata = gdata }
            -> s {
                game = (game s) {
                    stdata = gdata {
                        stevents = events
                    }
                }
            }
        _ -> error "parseStellarisScripts passed wrong kind of scripts!"
parseStellarisScripts _ = error "parseStellarisScripts passed wrong kind of scripts!"

writeStellarisScripts :: PPT IO ()
writeStellarisScripts = do
    writeStellarisEvents
