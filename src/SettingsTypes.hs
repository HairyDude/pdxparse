{-# LANGUAGE OverloadedStrings #-}
module SettingsTypes (
        L10n
    ,   CLArgs (..)
    ,   Game (..)
    ,   ScriptReader (..)
    ,   Handler (..)
    ,   Settings (
        -- Export everything EXCEPT L10n
            steamDir
        ,   steamApps
        ,   game
        ,   gameFolder
        ,   language
        ,   languageS
        ,   gameVersion
        ,   settingsFile
        ,   clargs
        ,   filesToProcess
        ,   currentFile
        ,   currentIndent
        )
    ,   settings
    ,   setGameL10n
    ,   PP, PPT
    ,   hoistErrors
    ,   indentUp, indentDown
    ,   withCurrentIndent, withCurrentIndentZero
    ,   alsoIndent, alsoIndent'
    ,   getGameL10n
    ,   getGameL10nDefault
    ,   getGameL10nIfPresent
    ,   withCurrentFile
    ,   getLangs
    ,   unfoldM, concatMapM
    ,   fromReaderT, toReaderT
    ) where

import Debug.Trace

import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader

import Data.Foldable (fold)
import Data.Maybe

import Data.Text (Text)
import Text.Shakespeare.I18N (Lang)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Abstract
import Doc
import EU4.Types
import Stellaris.Settings
import Yaml

-- Command line arguments.
data CLArgs
    = Paths
    | Version
    deriving (Show, Eq)

----------------------------
-- Game specific settings --
----------------------------

newtype ScriptReader = ScriptReader {
        runScriptReader :: Settings -> FilePath -> IO [(FilePath, GenericScript)]
    }
instance Show ScriptReader where
    show _ = "<script reader>"
newtype Handler = Handler {
        runHandler :: GenericStatement -> PPT (Either Text) [Either Text (FilePath, Doc)]
    }
instance Show Handler where
    show _ = "<feature handler>"

data Game
    = GameEU4 {
            gEU4 :: EU4
        ,   readScripts :: ScriptReader
        ,   features :: [FilePath]
        ,   handlers :: HashMap FilePath Handler
        }
    | GameStellaris {
            gStellaris :: Stellaris
        ,   readScripts :: ScriptReader
        ,   features :: [FilePath]
        ,   handlers :: HashMap FilePath Handler
        }
    deriving (Show)

----------------------
-- Generic settings --
----------------------

data Settings = Settings {
        steamDir    :: FilePath
    ,   steamApps   :: FilePath
    ,   game        :: Game
    ,   gameFolder  :: String
    ,   language    :: Text
    ,   languageS   :: String -- for FilePaths
    ,   gameVersion :: Text
    ,   gameL10n    :: L10n
    ,   langs       :: [Lang]
    ,   settingsFile :: FilePath
    ,   clargs      :: [CLArgs]
    ,   filesToProcess :: [FilePath]
    -- Local state
    ,   currentFile :: Maybe FilePath
    ,   currentIndent :: Maybe Int
    } deriving (Show)

-- All undefined/Nothing settings, except langs.
settings :: Settings
settings = Settings
    { steamDir       = error "steamDir not defined"
    , steamApps      = error "steamApps not defined"
    , game           = error "game not defined"
    , gameFolder     = error "gameFolder not defined"
    , language       = error "language not defined"
    , languageS      = error "languageS not defined"
    , gameVersion    = error "gameVersion not defined"
    , gameL10n       = error "gameL10n not defined"
    , currentFile    = error "currentFile not defined"
    , currentIndent  = error "currentIndent not defined"
    , langs          = ["en"]
    , settingsFile   = error "settingsFile not defined"
    , clargs         = []
    , filesToProcess = []
    }

setGameL10n :: Settings -> L10n -> Settings
setGameL10n settings l10n = settings { gameL10n = l10n }

-- Pretty-printing monad, and its transformer version
type PP a = Reader Settings a -- equal to PPT Identity a
type PPT m a = ReaderT Settings m a

-- Convert a PP wrapping errors into a PP returning Either.
-- TODO: generalize
hoistErrors :: Monad m => PPT (Either e) a -> PPT m (Either e a)
hoistErrors (ReaderT rd) = return . rd =<< ask

-- Increase current indentation by 1 for the given action.
-- If there is no current indentation, set it to 1.
indentUp :: Monad m => PPT m a -> PPT m a
indentUp go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 1 succ mindent)
    local (\s -> s { currentIndent = mindent' }) go

-- Decrease current indent level by 1 for the given action.
-- For use where a level of indentation should be skipped.
indentDown :: Monad m => PPT m a -> PPT m a
indentDown go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 0 pred mindent)
    local (\s -> s { currentIndent = mindent' }) go

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 1.
withCurrentIndent :: Monad m => (Int -> PPT m a) -> PPT m a
withCurrentIndent = withCurrentIndentBaseline 1

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 0.
withCurrentIndentZero :: Monad m => (Int -> PPT m a) -> PPT m a
withCurrentIndentZero = withCurrentIndentBaseline 0

withCurrentIndentBaseline :: Monad m => Int -> (Int -> PPT m a) -> PPT m a
withCurrentIndentBaseline base go =
    local (\s ->
            if isNothing (currentIndent s)
            then s { currentIndent = Just base }
            else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentIndent)

-- Bundle a value with the current indentation level.
alsoIndent :: Monad m => PPT m a -> PPT m (Int, a)
alsoIndent mx = withCurrentIndent $ \i -> mx >>= \x -> return (i,x)
alsoIndent' :: Monad m => a -> PPT m (Int, a)
alsoIndent' x = withCurrentIndent $ \i -> return (i,x)

getCurrentLang :: Monad m => PPT m L10nLang
getCurrentLang = HM.lookupDefault HM.empty <$> asks language <*> asks gameL10n

getGameL10n :: Monad m => Text -> PPT m Text
getGameL10n key = content <$> HM.lookupDefault (LocEntry 0 key) key <$> getCurrentLang

getGameL10nDefault :: Monad m => Text -> Text -> PPT m Text
getGameL10nDefault def key = content <$> HM.lookupDefault (LocEntry 0 def) key <$> getCurrentLang

getGameL10nIfPresent :: Monad m => Text -> PPT m (Maybe Text)
getGameL10nIfPresent key = fmap content <$> HM.lookup key <$> getCurrentLang

-- Pass the current file to the action.
-- If there is no current file, set it to "(unknown)".
withCurrentFile :: Monad m => (String -> PPT m a) -> PPT m a
withCurrentFile go = do
    mfile <- asks currentFile
    local (\s -> if isNothing mfile
                    then s { currentFile = Just "(unknown)" }
                    else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentFile)

-- Get the list of output languages.
getLangs :: Monad m => PPT m [Lang]
getLangs = asks langs

-- Misc. utilities

-- As unfoldr, but argument is monadic
unfoldM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldM f = go where
    go x = do
        res <- f x
        case res of
            Nothing -> return []
            Just (next, x') -> do
                rest <- go x'
                return (next:rest)

concatMapM :: (Monad m, Traversable t, Monoid (t b)) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f xs = liftM fold . mapM f $ xs

fromReaderT :: ReaderT r m a -> Reader r (m a)
fromReaderT mx = runReaderT mx <$> ask

toReaderT :: Reader r (m a) -> ReaderT r m a
toReaderT mx = ReaderT (runIdentity . runReaderT mx)
