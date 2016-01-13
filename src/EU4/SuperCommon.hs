{-# LANGUAGE OverloadedStrings #-}
-- Things that should be in Common.hs but need to be used by IdeaGroups.hs-boot.
module EU4.SuperCommon (
        MonarchPower (..)
    ,   AIWillDo (..)
    ,   AIModifier (..)
    ,   aiWillDo
    ) where

import Data.List (foldl')

import qualified Data.Text as T

import Abstract
-- Cannot import EU4.Common, EU4.IdeaGroups, or EU4.Decisions.

data MonarchPower = Administrative
                  | Diplomatic
                  | Military
    deriving (Show, Eq, Ord)

-- AI factors for idea groups and decisions

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
aiWillDo scr = foldl' aiWillDoAddSection newAIWillDo scr
aiWillDoAddSection :: AIWillDo -> GenericStatement -> AIWillDo
aiWillDoAddSection awd (Statement (GenericLhs left) right) = case T.toLower left of
    "factor" -> case floatRhs right of
        Just fac -> awd { awd_base = Just fac }
        _        -> awd
    "modifier" -> case right of
        CompoundRhs scr -> awd { awd_modifiers = awd_modifiers awd ++ [awdModifier scr] }
        _               -> awd
    _ -> awd
aiWillDoAddSection awd _ = awd

awdModifier :: GenericScript -> AIModifier
awdModifier scr = foldl' awdModifierAddSection newAIModifier scr
awdModifierAddSection :: AIModifier -> GenericStatement -> AIModifier
awdModifierAddSection aim stmt@(Statement (GenericLhs left) right) = case T.toLower left of
    "factor" -> case floatRhs right of
        Just fac -> aim { aim_factor = Just fac }
        Nothing  -> aim
    _ -> aim { aim_triggers = aim_triggers aim ++ [stmt] }
awdModifierAddSection aim _ = aim
