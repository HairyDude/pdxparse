{-# LANGUAGE OverloadedStrings #-}
module SettingsTypes
    ( L10n
    , Settings
        -- Export everything EXCEPT L10n
        ( steamDir
        , steamApps
        , game
        , language
        , gameVersion
        , currentFile
        , currentIndent
        , info
        )
    , settings
    , setGameL10n
    , PP, PPT
    , hoistErrors
    , indentUp, indentDown
    , withCurrentIndent, withCurrentIndentZero
    , alsoIndent, alsoIndent'
    , getGameL10n
    , getGameL10nDefault
    , getGameL10nIfPresent
    , withCurrentFile
    , getLangs
    , unfoldM, concatMapM 
    , fromReaderT, toReaderT
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

type L10n = HashMap Text Text

data Settings a = Settings {
        steamDir    :: FilePath
    ,   steamApps   :: FilePath
    ,   game        :: String
    ,   language    :: String
    ,   gameVersion :: Text
    ,   gameL10n    :: L10n
    ,   langs       :: [Lang]
    -- Local state
    ,   currentFile :: Maybe FilePath
    ,   currentIndent :: Maybe Int
    -- Extra information
    ,   info :: a
    } deriving (Show)

instance Functor Settings where
    fmap f s = s { info = f (info s) }

-- All undefined/Nothing settings, except langs.
settings :: a -> Settings a
settings x = Settings
    { steamDir = undefined
    , steamApps = undefined
    , game = undefined
    , language = undefined
    , gameVersion = undefined
    , gameL10n = undefined
    , currentFile = Nothing
    , currentIndent = Nothing
    , langs = ["en"]
    , info = x
    }

setGameL10n :: Settings a -> L10n -> Settings a
setGameL10n settings l10n = settings { gameL10n = l10n }

-- Pretty-printing monad, and its transformer version
type PP extra a = Reader (Settings extra) a -- equal to PPT extra Identity a
type PPT extra m a = ReaderT (Settings extra) m a

-- Convert a PP wrapping errors into a PP returning Either.
-- TODO: generalize
hoistErrors :: Monad m => PPT extra (Either e) a -> PPT extra m (Either e a)
hoistErrors (ReaderT rd) = return . rd =<< ask

-- Increase current indentation by 1 for the given action.
-- If there is no current indentation, set it to 1.
indentUp :: Monad m => PPT extra m a -> PPT extra m a
indentUp go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 1 succ mindent)
    local (\s -> s { currentIndent = mindent' }) go

-- Decrease current indent level by 1 for the given action.
-- For use where a level of indentation should be skipped.
indentDown :: Monad m => PPT extra m a -> PPT extra m a
indentDown go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 0 pred mindent)
    local (\s -> s { currentIndent = mindent' }) go

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 1.
withCurrentIndent :: Monad m => (Int -> PPT extra m a) -> PPT extra m a
withCurrentIndent = withCurrentIndentBaseline 1

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 0.
withCurrentIndentZero :: Monad m => (Int -> PPT extra m a) -> PPT extra m a
withCurrentIndentZero = withCurrentIndentBaseline 0

withCurrentIndentBaseline :: Monad m => Int -> (Int -> PPT extra m a) -> PPT extra m a
withCurrentIndentBaseline base go =
    local (\s ->
            if isNothing (currentIndent s)
            then s { currentIndent = Just base }
            else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentIndent)

-- Bundle a value with the current indentation level.
alsoIndent :: Monad m => PPT extra m a -> PPT extra m (Int, a)
alsoIndent mx = withCurrentIndent $ \i -> mx >>= \x -> return (i,x)
alsoIndent' :: Monad m => a -> PPT extra m (Int, a)
alsoIndent' x = withCurrentIndent $ \i -> return (i,x)

getGameL10n :: Monad m => Text -> PPT extra m Text
getGameL10n key = HM.lookupDefault key key <$> asks gameL10n

getGameL10nDefault :: Monad m => Text -> Text -> PPT extra m Text
getGameL10nDefault def key = HM.lookupDefault def key <$> asks gameL10n

getGameL10nIfPresent :: Monad m => Text -> PPT extra m (Maybe Text)
getGameL10nIfPresent key = HM.lookup key <$> asks gameL10n

-- Pass the current file to the action.
-- If there is no current file, set it to "(unknown)".
withCurrentFile :: Monad m => (String -> PPT extra m a) -> PPT extra m a
withCurrentFile go = do
    mfile <- asks currentFile
    local (\s -> if isNothing mfile
                    then s { currentFile = Just "(unknown)" }
                    else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentFile)

-- Get the list of output languages.
getLangs :: Monad m => PPT extra m [Lang]
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
