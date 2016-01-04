{-# LANGUAGE OverloadedStrings #-}
module EU4.IdeaGroups (
        IdeaGroup (..)
    ,   Idea (..)
    ,   IdeaTable
    ,   readIdeaGroup'
    ,   readIdeaGroupTable
    ) where

import Debug.Trace

import Control.Monad
import Control.Monad.Reader

import Data.Either
import Data.List

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import EU4.Common
import FileIO
import Messages
import SettingsTypes

import EU4.SuperCommon

-- Object that accumulates info about an idea group.
data IdeaGroup = IdeaGroup
    {   ig_name :: Text
    ,   ig_category :: Maybe MonarchPower
    ,   ig_bonus :: Maybe GenericScript
    ,   ig_trigger :: Maybe GenericScript
    ,   ig_ideas :: [Idea]
    ,   ig_ai_will_do :: Maybe AIWillDo
    } deriving (Show)
data Idea = Idea
    {   idea_name :: Text
    ,   idea_effects :: GenericScript
    } deriving (Show)
data AIWillDo = AIWillDo
    {   awd_base :: Maybe Double
    ,   awd_modifiers :: [AIModifier]
    } deriving (Show)
data AIModifier = AIModifier
    {   aim_factor :: Maybe Double
    ,   aim_triggers :: GenericScript
    } deriving (Show)
-- Starts off Nothing everywhere, except name (will get filled in immediately).
newIdeaGroup = IdeaGroup undefined Nothing Nothing Nothing [] Nothing
newAIWillDo = AIWillDo Nothing []
newAIModifier = AIModifier Nothing []

type IdeaTable = HashMap Text IdeaGroup

readIdeaGroupTable :: Settings a -> IO (HashMap Text IdeaGroup)
readIdeaGroupTable settings = do
    ideaGroupScripts <- readScript settings (buildPath settings "common/ideas/00_basic_ideas.txt")
    let (errs, ideaGroups) = partitionEithers $ map (readIdeaGroup' settings) ideaGroupScripts
    forM_ errs $ \err -> hPutStrLn stderr $ "Warning while parsing idea groups: " ++ T.unpack err
    return . HM.fromList . map (\ig -> (ig_name ig, ig)) $ ideaGroups

readIdeaGroup' :: Settings extra -> GenericStatement -> Either Text IdeaGroup
readIdeaGroup' settings stmt = runReader (readIdeaGroup stmt) settings

readIdeaGroup :: GenericStatement -> PP extra (Either Text IdeaGroup)
readIdeaGroup (StatementBare _) = return $ Left "bare statement at top level"
readIdeaGroup (Statement left right) = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> return $ Left "internal error: custom lhs"
        IntLhs _ -> return $ Left "int lhs at top level"
        GenericLhs name -> Right <$> foldM ideaGroupAddSection (newIdeaGroup { ig_name = name }) parts

    _ -> return $ Left "warning: unknown statement in idea group file"

ideaGroupAddSection :: IdeaGroup -> GenericStatement -> PP extra IdeaGroup
ideaGroupAddSection ig (Statement (GenericLhs label) rhs) = withCurrentFile $ \file ->
    case label of
        "category" -> case T.toLower <$> textRhs rhs of
            Just "adm" -> return ig { ig_category = Just Administrative }
            Just "dip" -> return ig { ig_category = Just Diplomatic }
            Just "mil" -> return ig { ig_category = Just Military }
            _          -> return ig
        "bonus" -> case rhs of
            CompoundRhs scr -> return ig { ig_bonus = Just scr }
            _               -> return ig
        "trigger" -> case rhs of
            CompoundRhs scr -> return ig { ig_trigger = Just scr }
            _               -> return ig
        "ai_will_do" -> case rhs of
            CompoundRhs scr -> return ig { ig_ai_will_do = Just (aiWillDo scr) }
            _               -> return ig
        _ -> case rhs of
            CompoundRhs scr -> return ig { ig_ideas = ig_ideas ig ++ [Idea label scr] }
            _               -> return ig

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

awdModifier :: GenericScript -> AIModifier
awdModifier scr = foldl' awdModifierAddSection newAIModifier scr
awdModifierAddSection :: AIModifier -> GenericStatement -> AIModifier
awdModifierAddSection aim stmt@(Statement (GenericLhs left) right) = case T.toLower left of
    "factor" -> case floatRhs right of
        Just fac -> aim { aim_factor = Just fac }
        Nothing  -> aim
    _ -> aim { aim_triggers = aim_triggers aim ++ [stmt] }

{-
pp_ideagroup :: IdeaGroup -> PP (Either Text Doc)
pp_ideagroup ig = do
-}
    
