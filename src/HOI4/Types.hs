{-# LANGUAGE OverloadedStrings #-}
module HOI4.Types (
        -- Used by Settings
        HOI4Data (..)
    ,   HOI4 (..)
    ,   HOI4Scripts (..)
        -- Features
    ,   HOI4EvtDesc (..), HOI4Event (..), HOI4Option (..)
        -- Low level
    ,   HOI4Scope (..)
    ,   Party (..)
--  ,   AIWillDo (..)
--  ,   AIModifier (..)
--  ,   aiWillDo
    ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)

import Abstract -- everything
--import Doc

--------------------------------------------
-- Types used by toplevel Settings module --
--------------------------------------------

data HOI4Data = HOI4Data {
        hoi4events :: HashMap Text HOI4Event
    -- etc.
    } deriving (Show)

-- State
data HOI4 = HOI4
    { scopeStack :: [HOI4Scope]
    } deriving (Show)

data HOI4Scripts = HOI4Scripts {
        hoi4eventScripts :: HashMap String GenericScript
    } deriving (Show)

-------------------
-- Feature types --
-------------------

data HOI4EvtDesc
    = HOI4EvtDescSimple Text  -- desc = key
    | HOI4EvtDescConditional GenericScript Text
              -- desc = { text = key trigger = conditions }
    | HOI4EvtDescCompound GenericScript
            -- desc = { trigger = { conditional_expressions } }
    deriving (Show)

-- Object that accumulates info about an event.
data HOI4Event = HOI4Event
    {   hoi4evt_id :: Maybe Text -- event id
    ,   hoi4evt_title :: Maybe Text -- event title l10n key
    ,   hoi4evt_desc :: [HOI4EvtDesc]
    ,   hoi4evt_picture :: Maybe Text -- event picture
    ,   hoi4evt_scope :: HOI4Scope -- type of thing the event happens to
    ,   hoi4evt_trigger :: Maybe GenericScript
    ,   hoi4evt_is_triggered_only :: Maybe Bool
    ,   hoi4evt_mean_time_to_happen :: Maybe GenericScript
    ,   hoi4evt_immediate :: Maybe GenericScript
    ,   hoi4evt_hide_window :: Bool
    ,   hoi4evt_options :: [HOI4Option]
    ,   hoi4evt_path :: Maybe FilePath -- source file
    } deriving (Show)
data HOI4Option = HOI4Option
    {   hoi4opt_name :: Maybe Text
    ,   hoi4opt_trigger :: Maybe GenericScript
    ,   hoi4opt_ai_chance :: Maybe GenericScript
    ,   hoi4opt_effects :: Maybe GenericScript
    } deriving (Show)

------------------------------
-- Shared lower level types --
------------------------------

-- TODO: expand these. Initial scopes assumed from event types.
data HOI4Scope
    = HOI4NoScope
    | HOI4Country
    | HOI4Province
    deriving (Show, Eq, Ord, Enum, Bounded)

data Party
    = Communism
    | Democratic
    | Fascism
    | Neutrality
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
