{-# LANGUAGE OverloadedStrings #-}
module EU4.Types (
        MonarchPower (..)
    ,   EU4Scope (..)
    ,   IdeaGroup (..)
    ,   Idea (..)
    ,   IdeaTable
    ,   EU4 (..)
    ,   AIWillDo (..)
    ,   AIModifier (..)
    ,   aiWillDo
    ) where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)

import Abstract

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

-- Object that accumulates info about an idea group.
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
    } deriving (Show)
data Idea = Idea
    {   idea_name :: Text
    ,   idea_name_loc :: Text
    ,   idea_effects :: GenericScript
    } deriving (Show)

-- State
type IdeaTable = HashMap Text IdeaGroup
data EU4 = EU4
    { scopeStack :: [EU4Scope]
    , ideas :: IdeaTable
    } deriving (Show)

-- AI decision factors

data AIWillDo = AIWillDo
    {   awd_base :: Maybe Double
    ,   awd_modifiers :: [AIModifier]
    } deriving (Show)
data AIModifier = AIModifier
    {   aim_factor :: Maybe Double
    ,   aim_triggers :: GenericScript
    } deriving (Show)
newAIWillDo = AIWillDo Nothing []
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
