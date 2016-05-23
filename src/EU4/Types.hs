module EU4.Types (
        EU4Scope (..)
    ,   IdeaGroup (..)
    ,   Idea (..)
    ,   IdeaTable
    ,   EU4 (..)
    ) where

import Data.Text (Text)
import Data.HashMap.Strict (HashMap)

import Abstract
import EU4.SuperCommon

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

