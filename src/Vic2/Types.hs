{-# LANGUAGE OverloadedStrings #-}
module Vic2.Types (
        -- Used by Settings
        Vic2Data (..)
    ,   Vic2 (..)
    ,   Vic2Scripts (..)
        -- Features
    ,   Vic2Event (..), Vic2Option (..)
    ,   Vic2Decision (..)
        -- Low level
    ,   Vic2Scope (..)
--    ,   AIWillDo (..)
--    ,   AIModifier (..)
--    ,   aiWillDo
    ) where

--import Data.List (foldl')
import Data.Text (Text)
--import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)

import Abstract
--import Doc

--------------------------------------------
-- Types used by toplevel Settings module --
--------------------------------------------

data Vic2Data = Vic2Data {
        vic2events :: HashMap Text Vic2Event
    ,   vic2decisions :: HashMap Text Vic2Decision
    -- etc.
    } deriving (Show)

-- State
data Vic2 = Vic2
    { scopeStack :: [Vic2Scope]
    } deriving (Show)

data Vic2Scripts = Vic2Scripts {
        vic2eventScripts :: HashMap String GenericScript
    ,   vic2decisionScripts :: HashMap String GenericScript
    } deriving (Show)

-------------------
-- Feature types --
-------------------

-- Object that accumulates info about an event.
data Vic2Event = Vic2Event
    {   vic2evt_id :: Maybe Text -- event id
    ,   vic2evt_title :: Maybe Text -- event title l10n key
    ,   vic2evt_desc :: Maybe Text -- event description l10n key
    ,   vic2evt_picture :: Maybe Text -- event picture
    ,   vic2evt_scope :: Vic2Scope -- type of thing the event happens to
    ,   vic2evt_trigger :: Maybe GenericScript
    ,   vic2evt_is_triggered_only :: Maybe Bool
    ,   vic2evt_mean_time_to_happen :: Maybe GenericScript
    ,   vic2evt_immediate :: Maybe GenericScript
    ,   vic2evt_options :: Maybe [Vic2Option]
    ,   vic2evt_path :: Maybe FilePath -- source file
    } deriving (Show)
data Vic2Option = Vic2Option
    {   vic2opt_name :: Maybe Text
    ,   vic2opt_trigger :: Maybe GenericScript
    ,   vic2opt_ai_chance :: Maybe GenericScript
    ,   vic2opt_effects :: Maybe GenericScript
    } deriving (Show)

-- Object that accumulates info about a decision.
data Vic2Decision = Vic2Decision
    {   dec_name :: Text
    ,   dec_name_loc :: Text
    ,   dec_text :: Maybe Text
    ,   dec_potential :: GenericScript
    ,   dec_allow :: GenericScript
    ,   dec_effect :: GenericScript
--  ,   dec_ai_will_do :: Maybe AIWillDo
    ,   dec_path :: Maybe FilePath -- source file
    } deriving (Show)

------------------------------
-- Shared lower level types --
------------------------------

data Vic2Scope
    = Vic2Country
    | Vic2Province
    | Vic2TradeNode
    | Vic2Geographic -- area, etc.
    | Vic2Bonus
    deriving (Show, Eq, Ord, Enum, Bounded)

-- AI decision factors
{- unused for now
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
