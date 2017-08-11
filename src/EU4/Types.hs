{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}
{-|
Module      : EU4.Types
Description : Types specific to Europa Universalis IV
-}
module EU4.Types (
        -- * Parser state
        EU4Data (..), EU4State (..)
    ,   EU4Info (..)
        -- * Features
    ,   EU4EvtDesc (..), EU4Event (..), EU4Option (..)
    ,   EU4Decision (..)
    ,   IdeaGroup (..), Idea (..), IdeaTable
        -- * Low level types
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
import SettingsTypes ( PPT, Settings
                     , IsGame (..), IsGameData (..), IsGameState (..))
--import Doc

--------------------------------------------
-- Types used by toplevel Settings module --
--------------------------------------------

-- | Settings, raw scripts, and parsed scripts.
data EU4Data = EU4Data {
        eu4settings :: Settings
    ,   eu4events :: HashMap Text EU4Event
    ,   eu4decisions :: HashMap Text EU4Decision
    ,   eu4ideaGroups :: IdeaTable
    ,   eu4eventScripts :: HashMap String GenericScript
    ,   eu4decisionScripts :: HashMap String GenericScript
    ,   eu4ideaGroupScripts :: HashMap String GenericScript
    -- etc.
    }

-- | State type for EU4.
data EU4State = EU4State {
        eu4scopeStack :: [EU4Scope]
    ,   eu4currentFile :: Maybe FilePath
    ,   eu4currentIndent :: Maybe Int
    } deriving (Show)

-- | Interface for EU4 feature handlers. Most of the methods just get data
-- tables from the parser state. These are empty until the relevant parsing
-- stages have been done. In order to avoid import loops, handlers don't know
-- the 'EU4.Settings.EU4' type itself, only its instances.
class (IsGame g,
       Scope g ~ EU4Scope,
       IsGameData (GameData g),
       IsGameState (GameState g)) => EU4Info g where
    -- | Get the title of an event by its ID. Only works if event scripts have
    -- been parsed.
    getEventTitle :: Monad m => Text -> PPT g m (Maybe Text)
    -- | Get the contents of all event script files.
    getEventScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Save (or amend) the contents of script event files in state.
    setEventScripts :: Monad m => HashMap FilePath GenericScript -> PPT g m ()
    -- | Get the parsed events table (keyed on event ID).
    getEvents :: Monad m => PPT g m (HashMap Text EU4Event)
    -- | Get the contents of all idea groups files.
    getIdeaGroupScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed idea groups table (keyed on idea group ID).
    getIdeaGroups :: Monad m => PPT g m IdeaTable
    -- | Get the contents of all decision script files.
    getDecisionScripts :: Monad m => PPT g m (HashMap FilePath GenericScript)
    -- | Get the parsed decisions table (keyed on decision ID).
    getDecisions :: Monad m => PPT g m (HashMap Text EU4Decision)

-------------------
-- Feature types --
-------------------

-- | Event description type. As of EU4 1.17, descriptions may be conditional.
data EU4EvtDesc
    = EU4EvtDescSimple Text  -- desc = key
    | EU4EvtDescConditional GenericScript Text
            -- desc = { text = key trigger = conditions }
    | EU4EvtDescCompound GenericScript
            -- desc = { trigger = { conditional_expressions } }
    deriving (Show)

-- | Event data.
data EU4Event = EU4Event {
    -- | Event ID
        eu4evt_id :: Maybe Text
    -- | Event title l10n key
    ,   eu4evt_title :: Maybe Text
    -- | Description
    ,   eu4evt_desc :: [EU4EvtDesc]
--  -- | Event picture
--  ,   eu4evt_picture :: Maybe Text
    -- | Type of thing the event happens to (e.g.  for a @country_event@ this
    -- is 'EU4Country'). This is used to set the top level scope for its
    -- scripts.
    ,   eu4evt_scope :: EU4Scope
    -- | What conditions allow the event to trigger.
    ,   eu4evt_trigger :: Maybe GenericScript
    -- | Whether the event is only triggered by script commands. If this is
    -- @False@ and the event also has a @mean_time_to_happen@, it can happen
    -- randomly.
    ,   eu4evt_is_triggered_only :: Maybe Bool
    -- | If this is a random event, how unlikely this event is to happen.
    ,   eu4evt_mean_time_to_happen :: Maybe GenericScript
    -- | Commands to execute as soon as the event fires.
    ,   eu4evt_immediate :: Maybe GenericScript
    -- | Whether this is a hidden event (it will have no options).
    ,   eu4evt_hide_window :: Bool
    -- | List of options for the player/AI to choose from.
    ,   eu4evt_options :: Maybe [EU4Option]
    -- | The event's source file.
    ,   eu4evt_path :: Maybe FilePath
    } deriving (Show)
-- | Event option data.
data EU4Option = EU4Option
    {   eu4opt_name :: Maybe Text               -- ^ Text of the option
    ,   eu4opt_trigger :: Maybe GenericScript   -- ^ Condition for the option to be available
    ,   eu4opt_ai_chance :: Maybe GenericScript -- ^ Probability that the AI will choose this option
    ,   eu4opt_effects :: Maybe GenericScript   -- ^ What happens if the player/AI chooses this option
    } deriving (Show)

-- | Table of idea groups, keyed by ID (e.g. @administrative_ideas@).
type IdeaTable = HashMap Text IdeaGroup
-- | Idea group data.
data IdeaGroup = IdeaGroup
    {   ig_name :: Text -- ^ Name of the idea group
    ,   ig_name_loc :: Text -- ^ Localized name of the idea group (in the best language)
    ,   ig_category :: Maybe MonarchPower -- ^ Which type of monarch power is used to buy these ideas
    ,   ig_start :: Maybe GenericScript -- ^ Traditions for a country idea group
    ,   ig_bonus :: Maybe GenericScript -- ^ Finisher / ambitions
    ,   ig_trigger :: Maybe GenericScript -- ^ Availability conditions if any
    ,   ig_free :: Bool -- ^ Whether this is a country idea group
    ,   ig_ideas :: [Idea] -- ^ List of ideas (there should always be 7)
    ,   ig_ai_will_do :: Maybe AIWillDo -- ^ Factors affecting whether AI will choose this group
    ,   ig_path :: Maybe FilePath -- ^ Source file
    } deriving (Show)
-- | Idea data.
data Idea = Idea
    {   idea_name :: Text -- ^ Idea ID
    ,   idea_name_loc :: Text -- ^ Localized idea name
    ,   idea_effects :: GenericScript -- ^ Idea effects (bonus scope)
    } deriving (Show)

-- | Decision data.
data EU4Decision = EU4Decision
    {   dec_name :: Text -- ^ Decision ID
    ,   dec_name_loc :: Text -- ^ Localized decision name
    ,   dec_text :: Maybe Text -- ^ Descriptive text (shown on hover)
    ,   dec_potential :: GenericScript -- ^ Conditions governing whether a
                                       --   decision shows up in the list
    ,   dec_allow :: GenericScript -- ^ Conditions that allow the player/AI to
                                   --   take the decision
    ,   dec_effect :: GenericScript -- ^ Effect on taking the decision
    ,   dec_ai_will_do :: Maybe AIWillDo -- ^ Factors affecting whether an AI
                                         --   will take the decision when available
    ,   dec_path :: Maybe FilePath -- ^ Source file
    } deriving (Show)

------------------------------
-- Shared lower level types --
------------------------------

-- | Types of monarch power.
data MonarchPower = Administrative
                  | Diplomatic
                  | Military
    deriving (Show, Eq, Ord)

-- | Scopes
data EU4Scope
    = EU4Country
    | EU4Province
    | EU4TradeNode
    | EU4Geographic -- ^ Area, etc.
    | EU4Bonus
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | AI decision factors.
data AIWillDo = AIWillDo
    {   awd_base :: Maybe Double
    ,   awd_modifiers :: [AIModifier]
    } deriving (Show)
-- | Modifiers for AI decision factors.
data AIModifier = AIModifier
    {   aim_factor :: Maybe Double
    ,   aim_triggers :: GenericScript
    } deriving (Show)
-- | Empty decision factor.
newAIWillDo :: AIWillDo
newAIWillDo = AIWillDo Nothing []
-- | Empty modifier.
newAIModifier :: AIModifier
newAIModifier = AIModifier Nothing []

-- | Parse an @ai_will_do@ clause.
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

-- | Parse a @modifier@ subclause for an @ai_will_do@ clause.
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

