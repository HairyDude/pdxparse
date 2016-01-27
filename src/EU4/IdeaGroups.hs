{-# LANGUAGE OverloadedStrings #-}
module EU4.IdeaGroups (
        IdeaGroup (..)
    ,   Idea (..)
    ,   readIdeaGroup'
    ,   readIdeaGroupTable
    ,   processIdeaGroup
    ) where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader

import Data.Array ((!))
import Data.Either
import Data.List
import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Text.Regex.TDFA (Regex)
import qualified Text.Regex.TDFA as RE

import Abstract
import Doc
import EU4.Common
import EU4.SuperCommon
import FileIO
import Messages
import SettingsTypes

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

-- Starts off Nothing everywhere, except name (will get filled in immediately).
newIdeaGroup = IdeaGroup undefined undefined Nothing Nothing Nothing Nothing False [] Nothing

readIdeaGroupTable :: Settings () -> IO IdeaTable
readIdeaGroupTable settings = do
    ideaGroupScripts <- readScript settings (buildPath settings "common/ideas/00_basic_ideas.txt")
    let (errs, ideaGroups) = partitionEithers $ map (readIdeaGroup' (const eu4 <$> settings)) ideaGroupScripts
    forM_ errs $ \err -> hPutStrLn stderr $ "Warning while parsing idea groups: " ++ T.unpack err
    return . HM.fromList . map (\ig -> (ig_name ig, ig)) $ ideaGroups

readIdeaGroup' :: Settings EU4 -> GenericStatement -> Either Text IdeaGroup
readIdeaGroup' settings stmt = runReader (readIdeaGroup stmt) settings

readIdeaGroup :: GenericStatement -> PP EU4 (Either Text IdeaGroup)
readIdeaGroup (StatementBare _) = return $ Left "bare statement at top level"
readIdeaGroup (Statement (GenericLhs "basic idea group") (GenericRhs right))
    -- This is a fake entry for an idea group that has already been parsed.
    -- Fetch the corresponding basic idea group from settings.
    = do
        groups <- getIdeas
        case HM.lookup right groups of
            Nothing -> return (Left $ "Idea group not found: " <> right)
            Just group -> return (Right group)
readIdeaGroup (Statement left right) = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> return $ Left "internal error: custom lhs"
        IntLhs _ -> return $ Left "int lhs at top level"
        GenericLhs name -> do
            name_loc <- getGameL10n name
            Right <$> foldM ideaGroupAddSection
                        (newIdeaGroup { ig_name = name
                                      , ig_name_loc = name_loc }) parts

    _ -> return $ Left "warning: unknown statement in idea group file"

ideaGroupAddSection :: IdeaGroup -> GenericStatement -> PP extra IdeaGroup
ideaGroupAddSection ig (Statement (GenericLhs label) rhs) =
    case label of
        "category" -> case T.toLower <$> textRhs rhs of
            Just "adm" -> return ig { ig_category = Just Administrative }
            Just "dip" -> return ig { ig_category = Just Diplomatic }
            Just "mil" -> return ig { ig_category = Just Military }
            _          -> return ig
        "start" -> case rhs of
            CompoundRhs scr -> return ig { ig_start = Just scr }
            _               -> return ig
        "bonus" -> case rhs of
            CompoundRhs scr -> return ig { ig_bonus = Just scr }
            _               -> return ig
        "trigger" -> case rhs of
            CompoundRhs scr -> return ig { ig_trigger = Just scr }
            _               -> return ig
        "ai_will_do" -> case rhs of
            CompoundRhs scr -> return ig { ig_ai_will_do = Just (aiWillDo scr) }
            _               -> return ig
        "free" -> case T.toLower <$> textRhs rhs of
            Just "yes" -> return ig { ig_free = True }
            _          -> return ig
        _ -> case rhs of
            CompoundRhs scr -> do
                ideaname_loc <- getGameL10n label
                return ig { ig_ideas = ig_ideas ig ++ [Idea label ideaname_loc scr] }
            _               -> return ig
ideaGroupAddSection ig  _ = return ig

-- Pick an icon for the idea, based on the first of its effects.
iconForIdea' :: Idea -> Maybe Text
iconForIdea' idea = case idea_effects idea of
    (Statement (GenericLhs eff) _:_) -> iconKey eff
    _ -> Nothing

iconForIdea :: Idea -> Doc
iconForIdea idea = case iconForIdea' idea of
    Nothing -> mempty
    Just icon -> strictText icon

processIdeaGroup :: GenericStatement -> PP EU4 (Either Text Doc)
processIdeaGroup stmt = do
    eig <- readIdeaGroup stmt
    case eig of
        Left err -> return (Left err)
        Right ig -> fmap fixup <$> ppIdeaGroup ig

-- Do some text substitutions to add 'ideaNicon' args to the idea group
-- template, and remove/comment out undesirable icon templates.
--
-- TODO: convert icon keys to icon file names.
-- XXX: do this properly in the first place.
fixup :: Doc -> Doc
fixup = strictText . T.unlines . map (TE.decodeUtf8
            -- . mungIdeaIcons multiIdeaIcons
            . mungIdeaIcons singleIdeaIcons
            . killIcons
        . TE.encodeUtf8) . T.lines . doc2text where
    badIcons, singleIdeaIcons, multiIdeaIcons{-, multiIdeaStartIcons -} :: Regex
    badIcons = RE.makeRegex ("((tradition.|bonus) = |\\* )({{icon[^}]*}}) "::ByteString)
    singleIdeaIcons = RE.makeRegex ("idea(.)effect = {{icon\\|([a-z ]*)\\|28px}} "::ByteString)
    multiIdeaIcons = RE.makeRegex ("(:)({{icon[^}]*}}) "::ByteString)
--    multiIdeaStartIcons = RE.makeRegex ("idea(.)effect = {{plainlist\\|\\* {{icon\\|([a-z ]*)\\|28px}} "::ByteString)
    killIcons :: ByteString -> ByteString
    killIcons s = case RE.matchOnceText badIcons s of
        Just (pre, matcharr, post) -> mconcat
            [pre, fst (matcharr ! 1)
            ,"<!-- ", fst (matcharr ! 3), " -->"
            ,post]
        Nothing -> case RE.matchOnceText multiIdeaIcons s of
            Just (pre, matcharr, post) -> mconcat
                [pre, fst (matcharr ! 1)
                ,"<!-- ", fst (matcharr ! 2), " -->"
                ,post]
            Nothing -> s
    mungIdeaIcons :: Regex -> ByteString -> ByteString
    mungIdeaIcons re s = case RE.matchOnceText re s of
        Nothing -> s
        Just (pre, matcharr, post) -> let nth = fst (matcharr ! 1) in mconcat
            [pre, "idea", nth, "icon = ", iconFileB (fst (matcharr ! 2))
            ,"\n| idea", nth, "effect = ", post]

ppIdeaGroup :: IdeaGroup -> PP EU4 (Either Text Doc)
ppIdeaGroup ig = do
    version <- asks gameVersion
    let name = ig_name_loc ig
    case (ig_bonus ig, length (ig_ideas ig)) of
        (Just bonus, 7) -> do
            let rawideas = ig_ideas ig
                unindent = map (first (const 0))
            ideas <- forM rawideas $ \idea -> do
                effmsgs <- ppMany (idea_effects idea)
                case effmsgs of
                    -- Remove the bullets from a single effect
                    [_] -> imsg2doc (unindent effmsgs)
                    {- This doesn't work, due to the template's abuse of
                       deflist markup and misbehaviour of MediaWiki.
                    -- Wrap multiple effects in a plainlist
                    _ -> do
                        effsdoc <- imsg2doc effmsgs
                        return $ templateDoc "plainlist" [effsdoc]
                    -}
                    -- Instead, replace bullets with colons.
                    [] -> return mempty
                    (m:ms) -> do
                        first <- imsg2doc (unindent [m])
                        rest <- mapM (\m -> (":" <>) <$> imsg2doc [m]) (unindent ms)
                        return (first <> line <> vsep rest)
            bonus_pp'd <- imsg2doc . unindent =<< ppMany bonus
            mtrigger_pp'd <- case ig_trigger ig of
                Nothing -> return Nothing
                Just trigger -> Just <$> (imsg2doc =<< ppMany trigger)
            let name_loc = strictText . T.replace " Ideas" "" $ name
                ig_id = strictText (ig_name ig)
            trads <- case ig_start ig of
                Just [trad1s, trad2s] -> do
                    trad1 <- imsg2doc . map (first (const 0)) =<< ppOne trad1s
                    trad2 <- imsg2doc . map (first (const 0)) =<< ppOne trad2s
                    return $ Right (trad1, trad2)
                Just trads -> return . Left . Just . length $ trads
                Nothing -> return (Left Nothing)
            return . Right . mconcat $
                ["<section begin=", ig_id, "/>", line
                ,"{{Idea group", line
                ,"| name = ", name_loc, line
                ,"| version = ", strictText version, line
                ,case ig_category ig of
                    Nothing -> case trads of
                        Right (trad1, trad2) -> mconcat -- assume groups with no category are country ideas
                            ["| country = yes", line
                            ,"| tradition1 = ", trad1, line
                            ,"| tradition2 = ", trad2, line
                            ]
                        Left (Just ntrads) -> mconcat
                            ["<!-- Looks like a country idea group, but has non-standard number of traditions ("
                            ,strictText (T.pack (show ntrads))
                            ,") -->", line]
                        Left Nothing -> mconcat
                            ["<!-- Looks like a country idea group, but has no traditions -->", line]
                    Just cat -> mconcat
                        ["<!-- Category: ", pp_string (show cat), " -->", line
                        ,"| events = ", name_loc, " idea group events", line]
                ,"| idea1 = ", strictText (idea_name_loc (rawideas !! 0)), line
                ,iconForIdea (rawideas !! 0)
                ,"| idea1effect = ", ideas !! 0, line
                ,"| idea2 = ", strictText (idea_name_loc (rawideas !! 1)), line
                ,iconForIdea (rawideas !! 1)
                ,"| idea2effect = ", ideas !! 1, line
                ,"| idea3 = ", strictText (idea_name_loc (rawideas !! 2)), line
                ,iconForIdea (rawideas !! 2)
                ,"| idea3effect = ", ideas !! 2, line
                ,"| idea4 = ", strictText (idea_name_loc (rawideas !! 3)), line
                ,iconForIdea (rawideas !! 3)
                ,"| idea4effect = ", ideas !! 3, line
                ,"| idea5 = ", strictText (idea_name_loc (rawideas !! 4)), line
                ,iconForIdea (rawideas !! 4)
                ,"| idea5effect = ", ideas !! 4, line
                ,"| idea6 = ", strictText (idea_name_loc (rawideas !! 5)), line
                ,iconForIdea (rawideas !! 5)
                ,"| idea6effect = ", ideas !! 5, line
                ,"| idea7 = ", strictText (idea_name_loc (rawideas !! 6)), line
                ,iconForIdea (rawideas !! 6)
                ,"| idea7effect = ", ideas !! 6, line
                ,"| bonus = ", bonus_pp'd, line
                ] ++ (case mtrigger_pp'd of
                    Just trigger_pp'd ->
                        ["| notes = Can be selected only if the following are true:", line
                        ,trigger_pp'd
                        ,line]
                    Nothing -> [])
                ++ ["}}", line
                ,"<section end=", ig_id, "/>"]
        (Nothing, _) -> return . Left $ "Idea group " <> name <> " has no bonus"
        (_, n) -> return . Left $ "Idea group " <> name <> " has non-standard number of ideas (" <> T.pack (show n) <> ")"
