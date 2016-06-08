{-# LANGUAGE OverloadedStrings #-}
module Stellaris.Types (
        -- Used by Settings
        StellarisData (..)
    ,   Stellaris (..)
    ,   StellarisScripts (..)
        -- Features
    ,   StEvtDesc (..), StellarisEvent (..), StellarisOption (..)
        -- Low level
    ,   StellarisScope (..)
--  ,   AIWillDo (..)
--  ,   AIModifier (..)
--  ,   aiWillDo
    ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)

import Abstract
--import Doc

--------------------------------------------
-- Types used by toplevel Settings module --
--------------------------------------------

data StellarisData = StellarisData {
        stevents :: HashMap Text StellarisEvent
    -- etc.
    } deriving (Show)

-- State
data Stellaris = Stellaris
    { scopeStack :: [StellarisScope]
    } deriving (Show)

data StellarisScripts = StellarisScripts {
        steventScripts :: HashMap String GenericScript
    } deriving (Show)

-------------------
-- Feature types --
-------------------

data StEvtDesc
    = StEvtDescSimple Text  -- desc = key
    | StEvtDescConditional GenericScript Text
            -- desc = { text = key trigger = conditions }
    | StEvtDescCompound GenericScript
            -- desc = { trigger = { conditional_expressions } }
    deriving (Show)

-- Object that accumulates info about an event.
data StellarisEvent = StellarisEvent
    {   stevt_id :: Maybe Text -- event id
    ,   stevt_title :: Maybe Text -- event title l10n key
    ,   stevt_desc :: [StEvtDesc]
    ,   stevt_picture :: Maybe Text -- event picture
    ,   stevt_scope :: StellarisScope -- type of thing the event happens to
    ,   stevt_trigger :: Maybe GenericScript
    ,   stevt_is_triggered_only :: Maybe Bool
    ,   stevt_mean_time_to_happen :: Maybe GenericScript
    ,   stevt_immediate :: Maybe GenericScript
    ,   stevt_hide_window :: Bool
    ,   stevt_options :: [StellarisOption]
    ,   stevt_path :: Maybe FilePath -- source file
    } deriving (Show)
data StellarisOption = StellarisOption
    {   stopt_name :: Maybe Text
    ,   stopt_trigger :: Maybe GenericScript
    ,   stopt_ai_chance :: Maybe GenericScript
    ,   stopt_effects :: Maybe GenericScript
    } deriving (Show)

------------------------------
-- Shared lower level types --
------------------------------

-- TODO: expand these. Initial scopes assumed from event types.
data StellarisScope
    = StellarisNoScope
    | StellarisCountry
    | StellarisFleet
    | StellarisPlanet
    | StellarisPop
    | StellarisPopFaction
    | StellarisShip
    | StellarisSystem
    | StellarisTile
    deriving (Show, Eq, Ord, Enum, Bounded)

-- AI decision factors

{- deferred
data AIWillDo = AIWillDo
    {   awd_base :: Maybe Double
    ,   awd_modifiers :: [AIModifier]
    } deriving (Show)
data AIModifier = AIModifier
    {   aim_factor :: Maybe Double
    ,   aim_triggers :: GenericScript
    } deriving (Show)
newAIWillDo :: AIWillDo
newAIWillDo = AIWillDo Nothing []
newAIModifier :: AIModifier
newAIModifier = AIModifier Nothing []

aiWillDo :: GenericScript -> AIWillDo
aiWillDo = foldl' aiWillDoAddSection newAIWillDo
aiWillDoAddSection :: AIWillDo -> GenericStatement -> AIWillDo
aiWillDoAddSection awd (Statement (GenericLhs left) OpEq right) = case T.toLower left of
    "factor" -> case floatRhs right of
        Just fac -> awd { awd_base = Just fac }
        _        -> awd
    "modifier" -> case right of
        CompoundRhs scr -> awd { awd_modifiers = awd_modifiers awd ++ [awdModifier scr] }
        _               -> awd
    _ -> awd
aiWillDoAddSection awd _ = awd

awdModifier :: GenericScript -> AIModifier
awdModifier = foldl' awdModifierAddSection newAIModifier
awdModifierAddSection :: AIModifier -> GenericStatement -> AIModifier
awdModifierAddSection aim stmt@(Statement (GenericLhs left) OpEq right) = case T.toLower left of
    "factor" -> case floatRhs right of
        Just fac -> aim { aim_factor = Just fac }
        Nothing  -> aim
    _ -> -- the rest of the statements are just the conditions.
        aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim _ = aim
-}
