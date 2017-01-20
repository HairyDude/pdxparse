{-# LANGUAGE OverloadedStrings #-}
module EU4.Types (
        -- Used by Settings
        EU4Data (..)
    ,   EU4 (..)
    ,   EU4Scripts (..)
        -- Features
    ,   EU4EvtDesc (..), EU4Event (..), EU4Option (..)
    ,   EU4Decision (..)
    ,   IdeaGroup (..), Idea (..), IdeaTable
        -- Low level
    ,   MonarchPower (..)
    ,   EU4Scope (..)
    ,   AIWillDo (..)
    ,   AIModifier (..)
    ,   aiWillDo
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

data EU4Data = EU4Data {
        eu4events :: HashMap Text EU4Event
    ,   eu4decisions :: HashMap Text EU4Decision
    ,   eu4ideagroups :: IdeaTable
    -- etc.
    } deriving (Show)

-- State
data EU4 = EU4
    { scopeStack :: [EU4Scope]
    } deriving (Show)

data EU4Scripts = EU4Scripts {
        eu4eventScripts :: HashMap String GenericScript
    ,   eu4decisionScripts :: HashMap String GenericScript
    ,   eu4ideaGroupScripts :: HashMap String GenericScript
    } deriving (Show)

-------------------
-- Feature types --
-------------------

data EU4EvtDesc
    = EU4EvtDescSimple Text  -- desc = key
    | EU4EvtDescConditional GenericScript Text
            -- desc = { text = key trigger = conditions }
    | EU4EvtDescCompound GenericScript
            -- desc = { trigger = { conditional_expressions } }
    deriving (Show)

-- Object that accumulates info about an event.
data EU4Event = EU4Event
    {   eu4evt_id :: Maybe Text -- event id
    ,   eu4evt_title :: Maybe Text -- event title l10n key
    ,   eu4evt_desc :: [EU4EvtDesc]
--    ,   eu4evt_picture :: Maybe Text -- event picture
    ,   eu4evt_scope :: EU4Scope -- type of thing the event happens to
    ,   eu4evt_trigger :: Maybe GenericScript
    ,   eu4evt_is_triggered_only :: Maybe Bool
    ,   eu4evt_mean_time_to_happen :: Maybe GenericScript
    ,   eu4evt_immediate :: Maybe GenericScript
    ,   eu4evt_hide_window :: Bool
    ,   eu4evt_options :: Maybe [EU4Option]
    ,   eu4evt_path :: Maybe FilePath -- source file
    } deriving (Show)
data EU4Option = EU4Option
    {   eu4opt_name :: Maybe Text
    ,   eu4opt_trigger :: Maybe GenericScript
    ,   eu4opt_ai_chance :: Maybe GenericScript
    ,   eu4opt_effects :: Maybe GenericScript
    } deriving (Show)

-- Object that accumulates info about an idea group.
type IdeaTable = HashMap Text IdeaGroup
data IdeaGroup = IdeaGroup
    {   ig_name :: Text
    ,   ig_name_loc :: Text
    ,   ig_category :: Maybe MonarchPower
    ,   ig_start :: Maybe GenericScript
    ,   ig_bonus :: Maybe GenericScript
    ,   ig_trigger :: Maybe GenericScript
    ,   ig_free :: Bool -- don't know what this means
    ,   ig_ideas :: [Idea]
    ,   ig_ai_will_do :: Maybe AIWillDo
    ,   ig_path :: Maybe FilePath -- source file
    } deriving (Show)
data Idea = Idea
    {   idea_name :: Text
    ,   idea_name_loc :: Text
    ,   idea_effects :: GenericScript
    } deriving (Show)

-- Object that accumulates info about a decision.
data EU4Decision = EU4Decision
    {   dec_name :: Text
    ,   dec_name_loc :: Text
    ,   dec_text :: Maybe Text
    ,   dec_potential :: GenericScript
    ,   dec_allow :: GenericScript
    ,   dec_effect :: GenericScript
    ,   dec_ai_will_do :: Maybe AIWillDo
    ,   dec_path :: Maybe FilePath -- source file
    } deriving (Show)

------------------------------
-- Shared lower level types --
------------------------------

data MonarchPower = Administrative
                  | Diplomatic
                  | Military
    deriving (Show, Eq, Ord)

data EU4Scope
    = EU4Country
    | EU4Province
    | EU4TradeNode
    | EU4Geographic -- area, etc.
    | EU4Bonus
    deriving (Show, Eq, Ord, Enum, Bounded)

-- AI decision factors

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

