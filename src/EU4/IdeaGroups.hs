{-# LANGUAGE OverloadedStrings, FlexibleContexts, QuasiQuotes #-}
module EU4.IdeaGroups (
        IdeaGroup (..)
    ,   Idea (..)
    ,   parseEU4IdeaGroups
    ,   writeEU4IdeaGroups 
    ) where

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Array ((!))
import Data.Either
import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Text.Regex.TDFA (Regex)
import qualified Text.Regex.TDFA as RE

import Debug.Trace

import Abstract
import Doc
import EU4.Common
import EU4.IO
--import EU4.SuperCommon
import Messages
import QQ
import SettingsTypes

-- Starts off Nothing everywhere, except name (will get filled in immediately).
newIdeaGroup :: IdeaGroup
newIdeaGroup = IdeaGroup undefined undefined Nothing Nothing Nothing Nothing False [] Nothing Nothing

parseEU4IdeaGroups :: Monad m => HashMap String GenericScript -> PPT m IdeaTable
parseEU4IdeaGroups ideaGroupScripts = do
    groupFiles <- sequenceA $ flip HM.mapWithKey ideaGroupScripts $ \path ideaGroupScript -> do
        -- For each file, parse the groups
        groupsWithErrors <- setCurrentFile path (mapM (runExceptT . parseIdeaGroup) ideaGroupScript)
        let (errs, groups) = partitionEithers groupsWithErrors
        forM_ errs $ \err -> traceM $ "Warning while parsing idea groups: " ++ err
        return . HM.fromList . map (\ig -> (ig_name ig, ig)) $ groups
    -- groupFiles :: HashMap String (HashMap Text IdeaGroup)
    -- This maps a filename to a map of id -> group.
    return (HM.unions (HM.elems groupFiles))

parseIdeaGroup :: Monad m =>
    GenericStatement -> ExceptT String (PPT m) IdeaGroup
parseIdeaGroup (StatementBare _) = throwError "bare statement at top level"
parseIdeaGroup [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        GenericLhs name -> do
            name_loc <- lift $ getGameL10n name
            ig <- foldM (curry (lift . uncurry ideaGroupAddSection))
                      (newIdeaGroup { ig_name = name
                                    , ig_name_loc = name_loc }) parts
            lift . withCurrentFile $ \file -> return (ig { ig_path = Just file })

    _ -> throwError "warning: unknown statement in idea group file"
parseIdeaGroup _ = error "idea group defined via operator other than ="

ideaGroupAddSection :: Monad m => IdeaGroup -> GenericStatement -> PPT m IdeaGroup
ideaGroupAddSection ig [pdx| $label = %rhs |] =
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
    (Statement (GenericLhs eff) OpEq _:_) -> iconKey eff
    _ -> Nothing

iconForIdea :: Idea -> Doc
iconForIdea idea = case iconForIdea' idea of
    Nothing -> mempty
    Just icon -> strictText icon

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

writeEU4IdeaGroups :: PPT IO ()
writeEU4IdeaGroups = do
    gdata <- gets game
    case gdata of
        GameEU4 { eu4data = EU4Data { eu4ideagroups = groups } } -> do
            writeFeatures "idea groups"
                          pathedGroups
                          ppIdeaGroup {- need IdeaGroup -> PPT IO (FilePath, Doc) -}
            where
                pathedGroups :: [Feature IdeaGroup]
                pathedGroups = map (\ig -> Feature {
                                            featurePath = ig_path ig
                                        ,   featureId = Just (ig_name ig)
                                        ,   theFeature = Right ig })
                                    (HM.elems groups)
        _ -> error "writeEU4IdeaGroups given non-EU4 state!"

ppIdeaGroup :: Monad m => IdeaGroup -> PPT (ExceptT Text m) Doc
ppIdeaGroup ig = fixup <$> do
    version <- gets gameVersion
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
                    (msg:msgs) -> do
                        firstmsg <- imsg2doc (unindent [msg])
                        rest <- mapM (\m -> (":" <>) <$> imsg2doc [m]) (unindent msgs)
                        return (firstmsg <> line <> vsep rest)
            bonus_pp'd <- imsg2doc . unindent =<< ppMany bonus
            mtrigger_pp'd <- case ig_trigger ig of
                Nothing -> return Nothing
                Just trigger -> Just <$> (imsg2doc =<< ppMany trigger)
            let name_loc = strictText . T.replace " Ideas" "" $ name
                ig_id_t = ig_name ig
                ig_id = strictText ig_id_t
            trads <- case ig_start ig of
                Just [trad1s, trad2s] -> do
                    trad1 <- imsg2doc . map (first (const 0)) =<< ppOne trad1s
                    trad2 <- imsg2doc . map (first (const 0)) =<< ppOne trad2s
                    return $ Right (trad1, trad2)
                Just trads -> return . Left . Just . length $ trads
                Nothing -> return (Left Nothing)
            return . mconcat $
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
        (Nothing, _) -> throwError $ "Idea group " <> name <> " has no bonus"
        (_, n) -> throwError $ "Idea group " <> name <> " has non-standard number of ideas (" <> T.pack (show n) <> ")"
