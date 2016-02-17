module EU4.IdeaGroups (
        IdeaGroup (..)
    ,   Idea (..)
    ) where

import Data.Text (Text)

import Data.HashMap.Strict (HashMap)

import Abstract
import EU4.Feature
import EU4.SuperCommon

data IdeaGroup = IdeaGroup
    {   ig_name :: Text
    ,   ig_name_loc :: Text
    ,   ig_path :: FilePath
    ,   ig_category :: Maybe MonarchPower
    ,   ig_start :: Maybe GenericScript
    ,   ig_bonus :: Maybe GenericScript
    ,   ig_trigger :: Maybe GenericScript
    ,   ig_free :: Bool -- don't know what this means
    ,   ig_ideas :: [Idea]
    ,   ig_ai_will_do :: Maybe AIWillDo
    }
data Idea = Idea
    {   idea_name :: Text
    ,   idea_name_loc :: Text
    ,   idea_effects :: GenericScript
    }
instance Show Idea

