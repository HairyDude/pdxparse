module EU4.IdeaGroups (
        IdeaGroup (..)
    ,   Idea (..)
    ,   IdeaTable
    ) where

import Data.Text (Text)

import Data.HashMap.Strict (HashMap)

import Abstract
import EU4.SuperCommon

data IdeaGroup = IdeaGroup
    {   ig_name :: Text
    ,   ig_category :: Maybe MonarchPower
    ,   ig_bonus :: Maybe GenericScript
    ,   ig_trigger :: Maybe GenericScript
    ,   ig_ideas :: [Idea]
    ,   ig_ai_will_do :: Maybe AIWillDo
    }
data Idea = Idea
    {   idea_name :: Text
    ,   idea_effects :: GenericScript
    }
instance Show Idea
data AIWillDo = AIWillDo
    {   awd_base :: Maybe Double
    ,   awd_modifiers :: [AIModifier]
    }
data AIModifier = AIModifier
    {   aim_factor :: Maybe Double
    ,   aim_triggers :: GenericScript
    }

type IdeaTable = HashMap Text IdeaGroup
