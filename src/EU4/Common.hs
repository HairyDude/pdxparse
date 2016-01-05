{-# LANGUAGE OverloadedStrings #-}
module EU4.Common (
        pp_script
    ,   pp_mtth
    ,   IdeaTable
    ,   module EU4.SuperCommon
    ) where

import Prelude hiding (sequence, mapM)

import Debug.Trace

import Control.Applicative
import Control.Monad.Reader hiding (sequence, mapM, forM)

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Traversable

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- TODO: get rid of these, do icon key lookups from another module
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Set (Set)
import qualified Data.Set as S

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), int, double)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Localization
import Messages
import EU4.SuperCommon
import {-# SOURCE #-} EU4.IdeaGroups

-- no particular order from here... TODO: organize this!

msgToPP :: ScriptMessage -> PP extra [(Int, ScriptMessage)]
msgToPP msg = (:[]) <$> alsoIndent' msg

isTag :: Text -> Bool
isTag s = T.length s == 3 && T.all isUpper s

isPronoun :: Text -> Bool
isPronoun s = T.map toLower s `S.member` pronouns where
    pronouns = S.fromList
        ["root"
        ,"prev"
        ,"owner"
        ,"controller"
        ]

pp_script :: GenericScript -> PP IdeaTable Doc
pp_script [] = return "(Nothing)"
pp_script script = withCurrentIndent $ \indent -> do
    statements_pp'd <- mapM pp_statement script
    return . hcat . punctuate line
        $ statements_pp'd

-- Get the localization for a province ID, if available.
getProvLoc :: Int -> PP extra Text
getProvLoc n =
    let provid_t = T.pack (show n)
    in getGameL10nDefault provid_t ("PROV" <> provid_t)


-- Emit flag template if the argument is a tag.
flag :: Text -> PP extra Doc
flag name =
    if isTag name
        then template "flag" . (:[]) <$> getGameL10n name
        else return $ case T.map toUpper name of
                "ROOT" -> "Our country" -- will need editing for capitalization in some cases
                "PREV" -> "Previously mentioned country"
                -- Suggestions of a description for FROM are welcome.
                _ -> strictText name

flagText :: Text -> PP extra Text
flagText = fmap doc2text . flag

-- Emit icon template.
icon :: Text -> Doc
icon what = template "icon" [HM.lookupDefault what what scriptIconTable, "28px"]
iconText :: Text -> Text
iconText = doc2text . icon

plainMsg :: Text -> PP extra [(Int, ScriptMessage)]
plainMsg msg = (:[]) <$> (alsoIndent' . MsgUnprocessed $ msg)

-- Surround a doc in a <pre> element.
pre_statement :: GenericStatement -> Doc
pre_statement stmt = "<pre>" <> genericStatement2doc stmt <> "</pre>"

preMessage :: GenericStatement -> ScriptMessage
preMessage = MsgUnprocessed . doc2text . pre_statement

preStatement :: GenericStatement -> PP extra [(Int, ScriptMessage)]
preStatement stmt = (:[]) <$> alsoIndent' (preMessage stmt)

-- Text version
pre_statement' :: GenericStatement -> Text
pre_statement' = doc2text . pre_statement

-- Pretty-print a statement, putting bullets in front of each line as appropiate.
pp_statement :: GenericStatement -> PP IdeaTable Doc
pp_statement stmt@(Statement lhs rhs) = post =<< ppOne stmt where 
    post :: [(Int, ScriptMessage)] -> PP extra Doc
    post msgs = vsep <$>
                    mapM (\(i,rm) -> do
                            m <- post' rm
                            return (hsep [strictText (T.replicate i "*"),  m]))
                         msgs
    post' :: ScriptMessage -> PP extra Doc
    post' msg = strictText <$> messageText msg

ppMany :: GenericScript -> PP IdeaTable [(Int, ScriptMessage)]
ppMany scr = concat <$> mapM ppOne scr

ppOne :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
ppOne stmt@(Statement lhs rhs) = case lhs of
    GenericLhs label -> case T.map toLower label of
        -- Statements where RHS is irrelevant (usually "yes")
        "add_cardinal"          -> msgToPP MsgAddCardinal
        "enable_hre_leagues"    -> msgToPP MsgEnableHRELeagues
        "kill_heir"             -> msgToPP MsgHeirDies
        "kill_ruler"            -> msgToPP MsgRulerDies
        "set_hre_religion_treaty" -> msgToPP MsgSignWestphalia
        "remove_cardinal"       -> msgToPP MsgLoseCardinal
        -- Gain/lose, with optional icon
        --  numbers
        "add_adm_power"         -> gainIcon True "adm" MsgGainADM stmt
        "add_army_tradition"    -> gainIcon True "army tradition" MsgGainAT stmt
        "add_authority"         -> gain' True MsgGainAuth stmt
        "add_base_tax"          -> gainIcon True "base tax" MsgGainBT stmt
        "add_base_production"   -> gainIcon True "base production" MsgGainBP stmt
        "add_base_manpower"     -> gainIcon True "base manpower" MsgGainBM stmt
        "add_dip_power"         -> gainIcon True "dip" MsgGainDIP stmt
        "add_doom"              -> gain' False MsgGainDoom stmt
        "add_heir_claim"        -> gain' True MsgHeirGainClaim stmt
        "add_devotion"          -> gainIcon True "devotion" MsgGainDevotion stmt
        "add_horde_unity"        -> gainIcon True "horde unity" MsgGainHordeUnity stmt
        "add_imperial_influence" -> gainIcon False "imperial authority" MsgGainImperialAuthority stmt
        "add_karma"              -> gainIcon True "high karma" MsgGainKarma stmt
        "add_legitimacy"         -> gainIcon True "legitimacy" MsgGainLegitimacy stmt
        "add_mil_power"          -> gainIcon True "mil" MsgGainMIL stmt
        "add_navy_tradition"     -> gainIcon True "navy tradition" MsgGainNavyTradition stmt
        "add_papal_influence"    -> gainIcon True "papal influence" MsgGainPapalInfluence stmt
        "add_prestige"           -> gainIcon True "prestige" MsgGainPrestige stmt
        "add_stability"          -> gainIcon True "stability" MsgGainStability stmt
        "add_war_exhaustion"     -> gainIcon False "war exhaustion" MsgGainWarExhaustion stmt
        "add_yearly_manpower"    -> gainIcon True "manpower" MsgGainYearlyManpower stmt
        "change_adm"             -> gainIcon True "adm" MsgGainADMSkill stmt
        "change_dip"             -> gainIcon True "dip" MsgGainDIPSkill stmt
        "change_mil"             -> gainIcon True "mil" MsgGainMILSkill stmt
        "change_siege"           -> gain' True MsgGainSiegeProgress stmt
        --  numbers
        "add_patriarch_authority"  -> gainIcon True "patriarch authority" MsgGainPatAuth stmt
        "add_piety"                -> gainIcon True "piety" MsgGainPiety stmt
        "add_republican_tradition" -> gainIcon True "republican tradition" MsgGainRepTrad stmt
        -- ages
        "add_inflation"      -> gainIcon False "inflation" MsgGainInflation stmt
        "add_local_autonomy" -> gainIcon False "local autonomy" MsgGainLocalAutonomy stmt
        "add_reform_desire"  -> gainIcon False "reform desire" MsgGainReformDesire stmt
        --  percentages
        "add_mercantilism"   -> gainIcon True "mercantilism" MsgGainMercantilism stmt
        -- Special
        "add_manpower" -> gainManpower stmt
        -- Modifiers
        "add_country_modifier"      -> addModifier MsgCountryMod stmt
        "add_permanent_province_modifier" -> addModifier MsgPermanentProvMod stmt
        "add_province_modifier"     -> addModifier MsgProvMod stmt
        "add_ruler_modifier"        -> addModifier MsgRulerMod stmt
        "add_trade_modifier"        -> addModifier MsgTradeMod stmt
        "has_country_modifier"      -> withLocAtom2 MsgCountryMod MsgHasModifier stmt
        "has_province_modifier"     -> withLocAtom2 MsgProvMod MsgHasModifier stmt
        "has_ruler_modifier"        -> withLocAtom2 MsgRulerMod MsgHasModifier stmt
        "has_trade_modifier"        -> withLocAtom2 MsgTradeMod MsgHasModifier stmt
        "remove_country_modifier"   -> withLocAtom2 MsgCountryMod MsgRemoveModifier stmt
        "remove_province_modifier"  -> withLocAtom2 MsgProvMod MsgRemoveModifier stmt
        -- Simple compound statements
        -- Note that "any" can mean "all" or "one or more" depending on context.
        "and"  -> compoundMessage MsgAllOf stmt
        "root" -> compoundMessage MsgOurCountry stmt
        -- These two are ugly, but without further analysis we can't know
        -- what it means.
        "from" -> compoundMessage MsgFROM stmt
        "prev" -> compoundMessage MsgPREV stmt
        "not"  -> compoundMessage MsgNoneOf stmt
        "or"   -> compoundMessage MsgAtLeastOneOf stmt
        -- There is a semantic distinction between "all" and "every",
        -- namely that the former means "this is true for all <type>" while
        -- the latter means "do this for every <type>." But their contexts
        -- are disjoint, so they can be presented the same way.
        "all_owned_province"        -> compoundMessage MsgEveryOwnedProvince stmt
        "any_active_trade_node"     -> compoundMessage MsgAnyActiveTradeNode stmt
        "any_ally"                  -> compoundMessage MsgAnyAlly stmt
        "any_core_country"          -> compoundMessage MsgAnyCoreCountry stmt
        "any_country"               -> compoundMessage MsgAnyCountry stmt
        "any_known_country"         -> compoundMessage MsgAnyKnownCountry stmt
        "any_neighbor_country"      -> compoundMessage MsgAnyNeighborCountry stmt
        "any_neighbor_province"     -> compoundMessage MsgAnyNeighborProvince stmt
        "any_owned_province"        -> compoundMessage MsgAnyOwnedProvince stmt
        "any_rival_country"         -> compoundMessage MsgAnyRival stmt
        "capital_scope"             -> compoundMessage MsgCapital stmt
        "controller"                -> compoundMessage MsgController stmt
        "emperor"                   -> compoundMessage MsgEmperor stmt
        "every_country"             -> compoundMessage MsgEveryCountry stmt
        "every_enemy_country"       -> compoundMessage MsgEveryEnemyCountry stmt
        "every_known_country"       -> compoundMessage MsgEveryKnownCountry stmt
        "every_neighbor_country"    -> compoundMessage MsgEveryNeighborCountry stmt
        "every_neighbor_province"   -> compoundMessage MsgEveryNeighborProvince stmt
        "every_owned_province"      -> compoundMessage MsgEveryOwnedProvince stmt
        "every_province"            -> compoundMessage MsgEveryProvince stmt
        "every_subject_country"     -> compoundMessage MsgEverySubject stmt
        "hidden_effect"             -> compoundMessage MsgHiddenEffect stmt
        "if"                        -> compoundMessage MsgIf stmt
        "limit"                     -> compoundMessage MsgLimit stmt
        "owner"                     -> compoundMessage MsgOwner stmt
        "random_ally"               -> compoundMessage MsgRandomAlly stmt
        "random_core_country"       -> compoundMessage MsgRandomCoreCountry stmt
        "random_country"            -> compoundMessage MsgRandomCountry stmt
        "random_list"               -> compoundMessage MsgRandom stmt
        "random_neighbor_country"   -> compoundMessage MsgRandomNeighborCountry stmt
        "random_neighbor_province"  -> compoundMessage MsgRandomNeighborProvince stmt
        "random_owned_province"     -> compoundMessage MsgRandomOwnedProvince stmt
        "random_province"           -> compoundMessage MsgRandomProvince stmt
        -- Random
        "random" -> random stmt
        -- Simple generic statements (RHS is a localizable atom)
        "change_government" -> withLocAtom MsgChangeGovernment stmt
        "continent"         -> withLocAtom MsgContinentIs stmt
        "culture"           -> withLocAtom MsgCultureIs stmt
        "culture_group"     -> withLocAtom MsgCultureIsGroup stmt
        "dynasty"           -> withLocAtom MsgRulerIsDynasty stmt
        "end_disaster"      -> withLocAtom MsgDisasterEnds stmt
        "government"        -> withLocAtom MsgGovernmentIs stmt
        "has_advisor"       -> withLocAtom MsgHasAdvisor stmt
        "has_disaster"      -> withLocAtom MsgDisasterOngoing stmt
        "has_idea"          -> withLocAtom MsgHasIdea stmt
        "has_terrain"       -> withLocAtom MsgHasTerrain stmt 
        "infantry"          -> withLocAtom MsgInfantrySpawns stmt
        "kill_advisor"      -> withLocAtom MsgAdvisorDies stmt
        "primary_culture"   -> withLocAtom MsgPrimaryCultureIs stmt
        "region"            -> withLocAtom MsgRegionIs stmt
        "remove_advisor"    -> withLocAtom MsgLoseAdvisor stmt
        -- RHS is a province ID
        "province_id"   -> withProvince MsgProvinceIs stmt
        "owns"          -> withProvince MsgOwns stmt
        -- RHS is an advisor ID (TODO: parse advisor files)
        "advisor_exists"      -> numeric MsgAdvisorExists stmt
        "is_advisor_employed" -> numeric MsgAdvisorIsEmployed stmt
        -- Simple generic statements (typewriter face)
        "clr_country_flag"  -> withNonlocAtom2 MsgCountryFlag MsgClearFlag stmt
        "clr_province_flag" -> withNonlocAtom2 MsgProvinceFlag MsgClearFlag stmt
        "clr_ruler_flag"    -> withNonlocAtom2 MsgRulerFlag MsgClearFlag stmt
        "has_country_flag"  -> withNonlocAtom2 MsgCountryFlag MsgHasFlag stmt
        "has_global_flag"   -> withNonlocAtom2 MsgGlobalFlag MsgHasFlag stmt
        "has_province_flag" -> withNonlocAtom2 MsgProvinceFlag MsgHasFlag stmt
        "has_ruler_flag"    -> withNonlocAtom2 MsgRulerFlag MsgHasFlag stmt
        "set_country_flag"  -> withNonlocAtom2 MsgCountryFlag MsgSetFlag stmt
        "set_global_flag"   -> withNonlocAtom2 MsgGlobalFlag MsgSetFlag stmt
        "set_province_flag" -> withNonlocAtom2 MsgProvinceFlag MsgSetFlag stmt
        "set_ruler_flag"    -> withNonlocAtom2 MsgRulerFlag MsgSetFlag stmt
        -- Simple generic statements with icon
        "advisor"            -> withLocAtomIcon MsgHasAdvisorType stmt
        "change_trade_goods" -> withLocAtomIcon MsgChangeGoods stmt
        "create_advisor"     -> withLocAtomIcon MsgCreateAdvisor stmt
        "dominant_religion"  -> withLocAtomIcon MsgDominantReligion stmt
        "has_building"       -> withLocAtomIcon MsgHasBuilding stmt
        "has_idea_group"     -> withLocAtomIcon MsgHasIdeaGroup stmt
        "hre_religion"       -> withLocAtomIcon MsgHREReligion stmt
        "remove_estate"      -> withLocAtomIcon MsgRemoveFromEstate stmt 
        "set_hre_heretic_religion" -> withLocAtomIcon MsgSetHREHereticReligion stmt
        "set_hre_religion"   -> withLocAtomIcon MsgSetHREReligion stmt
        "technology_group"   -> withLocAtomIcon MsgTechGroup stmt
        "trade_goods"        -> withLocAtomIcon MsgProducesGoods stmt
        "has_estate"         -> withLocAtomIcon MsgHasEstate stmt -- Country scope "estate exists", province "assigned to estate"
        "set_estate"         -> withLocAtomIcon MsgAssignToEstate stmt
        "is_monarch_leader"  -> withLocAtomAndIcon "ruler general" MsgRulerIsGeneral stmt
        -- Simple generic statements with flag
        "alliance_with"      -> withFlag MsgAlliedWith stmt
        "cede_province"      -> withFlag MsgCedeProvinceTo stmt
        "controlled_by"      -> withFlag MsgControlledBy stmt
        "defensive_war_with" -> withFlag MsgDefensiveWarAgainst stmt
        "discover_country"   -> withFlag MsgDiscoverCountry stmt
        "add_claim"          -> withFlag MsgGainClaim stmt
        "has_discovered"     -> withFlag MsgHasDiscovered stmt
        "inherit"            -> withFlag MsgInherit stmt
        "is_neighbor_of"     -> withFlag MsgNeighbors stmt
        "is_league_enemy"    -> withFlag MsgIsLeagueEnemy stmt
        "is_subject_of"      -> withFlag MsgIsSubjectOf stmt
        "remove_core"        -> withFlag MsgLoseCore stmt
        "marriage_with"      -> withFlag MsgRoyalMarriageWith stmt
        "offensive_war_with" -> withFlag MsgOffensiveWarAgainst stmt
        "owned_by"           -> withFlag MsgOwnedBy stmt
        "release"            -> withFlag MsgReleaseVassal stmt
        "sieged_by"          -> withFlag MsgUnderSiegeBy stmt
        "is_strongest_trade_power" -> withFlag MsgStrongestTradePower stmt
        "tag"                -> withFlag MsgCountryIs stmt
        "truce_with"         -> withFlag MsgTruceWith stmt
        "war_with"           -> withFlag MsgAtWarWith stmt
        "white_peace"        -> withFlag MsgMakeWhitePeace stmt
        -- Simple generic statements with flag or "yes"/"no"
        "exists"            -> withFlag MsgCountryExists stmt
        -- Statements that may be an icon, a flag, or a pronoun (such as ROOT)
        -- Boolean argument is whether to emit an icon.
        "religion"          -> iconOrFlag MsgReligion MsgSameReligion stmt
        "religion_group"    -> iconOrFlag MsgReligionGroup MsgSameReligionGroup stmt
        "change_religion"   -> iconOrFlag MsgChangeReligion MsgChangeSameReligion stmt
        -- Statements that may be either a tag or a province
        "is_core"  -> tagOrProvince MsgIsCoreOf MsgHasCoreOn stmt
        "is_claim" -> tagOrProvince MsgHasClaim MsgHasClaimOn stmt
        -- Boolean statements
        "ai"                     -> withBool MsgIsAIControlled stmt
        "has_cardinal"           -> withBool MsgHasCardinal stmt
        "has_heir"               -> withBool MsgHasHeir stmt
        "has_owner_religion"     -> withBool MsgHasOwnerReligion stmt
        "has_port"               -> withBool MsgHasPort stmt
        "has_seat_in_parliament" -> withBool MsgHasSeatInParliament stmt
        "has_regency"            -> withBool MsgIsInRegency stmt
        "has_siege"              -> withBool MsgUnderSiege stmt
        "hre_leagues_enabled"    -> withBool MsgHRELeaguesEnabled stmt
        "hre_religion_locked"    -> withBool MsgHREReligionLocked stmt
        "hre_religion_treaty"    -> withBool MsgHREWestphalia stmt
        "is_at_war"              -> withBool MsgAtWar stmt
        "is_capital"             -> withBool MsgIsCapital stmt
        "is_city"                -> withBool MsgIsCity stmt
        "is_elector"             -> withBool MsgIsElector stmt
        "is_emperor"             -> withBool MsgIsEmperor stmt
        "is_female"              -> withBool MsgIsFemale stmt
        "is_in_league_war"       -> withBool MsgIsInLeagueWar stmt
        "is_lesser_in_union"     -> withBool MsgIsLesserInUnion stmt
        "is_looted"              -> withBool MsgIsLooted stmt
        "is_overseas"            -> withBool MsgIsOverseas stmt
        "is_part_of_hre"         -> withBool MsgIsPartOfHRE stmt
        "is_reformation_center"  -> withBool MsgIsCenterOfReformation stmt
        "is_subject"             -> withBool MsgIsSubject stmt
        "papacy_active"          -> withBool MsgPapacyIsActive stmt
        "set_hre_religion_locked" -> withBool MsgSetHREReligionLocked stmt
        "unit_in_siege"          -> withBool MsgUnderSiege stmt -- duplicate?
        "was_player"             -> withBool MsgHasBeenPlayer stmt
        -- Numeric statements
        "colonysize"                -> numeric MsgColonySettlers stmt
        "had_recent_war"            -> numeric MsgWasAtWar stmt
        "heir_age"                  -> numeric MsgHeirAge stmt
        "is_year"                   -> numeric MsgYearIs stmt
        "num_of_loans"              -> numeric MsgNumLoans stmt
        "num_of_mercenaries"        -> numeric MsgNumMercs stmt
        "num_of_ports"              -> numeric MsgNumPorts stmt
        "num_of_rebel_armies"       -> numeric MsgNumRebelArmies stmt
        "num_of_trade_embargos"     -> numeric MsgNumEmbargoes stmt
        "units_in_province"         -> numeric MsgUnitsInProvince stmt
        -- Statements that may be numeric or a tag
        "num_of_cities"             -> numericOrTag MsgNumCities MsgNumCitiesThan stmt
        -- Signed numeric statements
        "tolerance_to_this" -> numeric MsgToleranceToThis stmt
        -- Statements of numeric quantities with icons
        "add_years_of_income" -> numericIcon "ducats" MsgAddYearsOfIncome stmt
        "adm"               -> numericIcon "adm" MsgRulerADM stmt
        "adm_tech"          -> numericIcon "adm tech" MsgADMTech stmt
        "army_tradition"    -> numericIcon "army tradition" MsgArmyTradition stmt
        "base_manpower"     -> numericIcon "navy tradition" MsgBaseManpower stmt
        "base_production"   -> numericIcon "base production" MsgBaseProduction stmt
        "base_tax"          -> numericIcon "base tax" MsgBaseTax stmt
        "create_admiral"    -> numericIcon "admiral" MsgCreateAdmiral stmt
        "create_general"    -> numericIcon "general" MsgCreateGeneral stmt
        "development"       -> numericIcon "development" MsgDevelopment stmt
        "dip"               -> numericIcon "dip" MsgRulerDIP stmt
        "dip_tech"          -> numericIcon "dip tech" MsgDIPTech stmt
        "horde_unity"       -> numericIcon "horde unity" MsgHordeUnity stmt
        "karma"             -> numericIcon "high karma" MsgKarma stmt
        "legitimacy"        -> numericIcon "legitimacy" MsgLegitimacy stmt
        "mil"               -> numericIcon "mil" MsgRulerMIL stmt
        "mil_tech"          -> numericIcon "mil tech" MsgMILTech stmt
        "monthly_income"    -> numericIcon "ducats" MsgMonthlyIncome stmt
        "num_of_allies"     -> numericIcon "alliance" MsgNumAllies stmt
        "num_of_cardinals"  -> numericIcon "cardinal" MsgNumCardinals stmt
        "num_of_colonists"  -> numericIcon "colonists" MsgNumColonists stmt
        "stability"         -> numericIcon "stability" MsgStability stmt
        "total_development" -> numericIcon "development" MsgTotalDevelopment stmt
        "total_number_of_cardinals" -> numericIcon "cardinal" MsgTotalCardinals stmt
        "unrest"               -> numericIcon "unrest" MsgUnrest stmt
        "war_exhaustion"       -> numericIcon "war exhaustion" MsgWarExhaustion stmt
        "war_score"            -> numericIcon "war score" MsgWarScore stmt
        "republican_tradition" -> numericIcon "republican tradition" MsgRepTrad stmt
        "inflation"            -> numericIcon "inflation" MsgInflation stmt
        "local_autonomy"       -> numericIcon "local autonomy" MsgLocalAutonomy stmt
        "manpower_percentage"  -> numericIcon "manpower" MsgManpowerPercentage stmt
        "mercantilism"         -> numericIcon "mercantilism" MsgMercantilism stmt
        -- Complex statements
        "add_casus_belli"                   -> addCB True stmt
        "add_faction_influence"             -> factionInfluence stmt
        "add_estate_loyalty"                -> estateLoyalty MsgAddEstateLoyalty stmt
        "add_estate_influence_modifier"     -> estateInfluenceModifier MsgEstateInfluenceModifier stmt
        "add_opinion"                       -> opinion MsgAddOpinion MsgAddOpinionDur stmt
        "reverse_add_opinion"               -> opinion MsgReverseAddOpinion MsgReverseAddOpinionDur stmt
        "area"                              -> case rhs of
            CompoundRhs _ -> compoundMessage MsgArea stmt
            _             -> withLocAtom MsgAreaIs stmt
        "define_heir"                       -> defineHeir stmt
        "build_to_forcelimit"               -> buildToForcelimit stmt
        "country_event"                     -> triggerEvent MsgCountryEvent stmt
        "declare_war_with_cb"               -> declareWarWithCB stmt
        "define_advisor"                    -> defineAdvisor stmt
        "define_ruler"                      -> defineRuler stmt
        "estate_influence"                  -> estateInfluence MsgEstateInfluence stmt
        "estate_loyalty"                    -> estateLoyalty MsgEstateLoyalty stmt
        "had_country_flag"                  -> hadFlag MsgCountryFlag stmt
        "had_global_flag"                   -> hadFlag MsgGlobalFlag stmt
        "had_province_flag"                 -> hadFlag MsgProvinceFlag stmt
        "had_ruler_flag"                    -> hadFlag MsgRulerFlag stmt
        "has_estate_influence_modifier"     -> hasEstateInfluenceModifier stmt
        "has_opinion_modifier"              -> opinion MsgHasOpinion (\what who _years -> MsgHasOpinion what who) stmt
        "province_event"                    -> triggerEvent MsgProvinceEvent stmt
        "remove_opinion"                    -> opinion MsgRemoveOpinion (\what who _years -> MsgRemoveOpinion what who) stmt
        "religion_years"                    -> religionYears stmt
        "reverse_add_casus_belli"           -> addCB False stmt
        "trigger_switch"                    -> triggerSwitch stmt
        "num_of_religion"                   -> numOfReligion stmt
        -- Rebels
        "can_spawn_rebels"   -> canSpawnRebels stmt
        "create_revolt"      -> spawnRebels Nothing stmt
        "has_spawned_rebels" -> hasSpawnedRebels stmt
        "likely_rebels"      -> canSpawnRebels stmt
        "nationalist_rebels" -> spawnRebels (Just "nationalist_rebels") stmt
        "spawn_rebels"       -> spawnRebels Nothing stmt
        -- Idea groups
        "innovativeness_ideas" -> hasIdea MsgHasInnovativenessIdea stmt
        -- Special
        "add_core"  -> addCore stmt
        "has_dlc"   -> hasDlc stmt
        "hre_reform_level" -> hreReformLevel stmt
        -- Ignored
        "custom_tooltip" -> plainMsg "(custom tooltip - delete this line)"
        "tooltip" -> plainMsg "(explanatory tooltip - delete this line)"
        -- default
        _ -> if isTag label
             then case rhs of
                CompoundRhs scr -> do
                    [lflag] <- plainMsg =<< (<> ":") <$> flagText label
                    scriptMsgs <- indentUp (ppMany scr)
                    return (lflag : scriptMsgs)
                _ -> preStatement stmt
             -- foo = N where foo is localizable and has an icon
             else case (numLhsMsg label, floatRhs rhs::Maybe Double) of
                (Just msg, Just _) -> numericIcon2 msg stmt
                _ -> do
                    mloc <- getGameL10nIfPresent label
                    case mloc of
                        -- Check for localizable atoms, e.g. regions
                        Just loc -> compound loc stmt
                        Nothing -> preStatement stmt
    IntLhs n -> do -- Treat as a province tag
        let provN = T.pack (show n)
        prov_loc <- getGameL10nDefault ("Province " <> provN) ("PROV" <> provN)
        case rhs of
            CompoundRhs scr -> do
                header <- msgToPP (MsgProvince prov_loc)
                scriptMsgs <- indentUp (ppMany scr)
                return (header ++ scriptMsgs)
            _ -> preStatement stmt


------------------------------------------------------------------------
-- Script handlers that should be used directly, not via pp_statement --
------------------------------------------------------------------------

data MTTH = MTTH
        {   years :: Maybe Int
        ,   months :: Maybe Int
        ,   days :: Maybe Int
        ,   modifiers :: [MTTHModifier] -- TODO
        } deriving Show
data MTTHModifier = MTTHModifier
        {   mtthmod_factor :: Maybe Double
        ,   mtthmod_conditions :: GenericScript
        } deriving Show
newMTTH = MTTH Nothing Nothing Nothing []
newMTTHMod = MTTHModifier Nothing []
pp_mtth :: GenericScript -> PP IdeaTable Doc
pp_mtth scr
    = pp_mtth' $ foldl' addField newMTTH scr
    where
        addField mtth (Statement (GenericLhs "years") (IntRhs n))
            = mtth { years = Just n }
        addField mtth (Statement (GenericLhs "years") (FloatRhs n))
            = mtth { years = Just (floor n) }
        addField mtth (Statement (GenericLhs "months") (IntRhs n))
            = mtth { months = Just n }
        addField mtth (Statement (GenericLhs "months") (FloatRhs n))
            = mtth { months = Just (floor n) }
        addField mtth (Statement (GenericLhs "days") (IntRhs n))
            = mtth { days = Just n }
        addField mtth (Statement (GenericLhs "days") (FloatRhs n))
            = mtth { days = Just (floor n) }
        addField mtth (Statement (GenericLhs "modifier") (CompoundRhs rhs))
            = addMTTHMod mtth rhs
        addField mtth _ = mtth -- unrecognized
        addMTTHMod mtth scr = mtth { modifiers = modifiers mtth ++ [foldl' addMTTHModField newMTTHMod scr] } where
            addMTTHModField mtthmod (Statement (GenericLhs "factor") rhs)
                = mtthmod { mtthmod_factor = floatRhs rhs }
            addMTTHModField mtthmod stmt -- anything else is a condition
                = mtthmod { mtthmod_conditions = mtthmod_conditions mtthmod ++ [stmt] }
        pp_mtth' mtth@(MTTH years months days modifiers) = do
            modifiers_pp'd <- indentUp (intersperse line <$> mapM pp_mtthmod modifiers)
            let hasYears = isJust years
                hasMonths = isJust months
                hasDays = isJust days
                hasModifiers = not (null modifiers)
            return . mconcat $
                ["*"]
                ++
                ((if hasYears then
                    [PP.int (fromJust years), space, "year(s)"]
                    ++
                    if hasMonths && hasDays then [",", space]
                    else if hasMonths || hasDays then ["and", space]
                    else []
                 else [])
                ++
                (if hasMonths then
                    [PP.int (fromJust months), space, "month(s)"]
                 else [])
                ++
                (if hasDays then
                    (if hasYears && hasMonths then ["and", space]
                     else []) -- if years but no months, already added "and"
                    ++
                    [PP.int (fromJust days), space, "day(s)"]
                 else []))
                ++
                (if hasModifiers then
                    [line, "''Modifiers''", line]
                    ++ modifiers_pp'd
                 else [])
        pp_mtthmod mtthmod@(MTTHModifier (Just factor) conditions) = do
            conditions_pp'd <- indentUp (pp_script conditions)
            return . mconcat $
                ["*"
                ,enclose "'''×" "''':" (pp_float factor)
                ,line
                ,conditions_pp'd
                ]
        pp_mtthmod mtthmod@(MTTHModifier Nothing conditions)
            = return "(invalid modifier! Bug in extractor?)"

--------------------------------
-- General statement handlers --
--------------------------------

compound :: Text -> GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
compound header (Statement _ (CompoundRhs scr)) = do
    headerMsg <- plainMsg (header <> ":")
    scriptMsgs <- indentUp (ppMany scr)
    return $ headerMsg ++ scriptMsgs
compound defaultdoc stmt = preStatement stmt

compoundMessage :: ScriptMessage -> GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
compoundMessage header (Statement _ (CompoundRhs scr)) = do
        script_pp'd <- indentUp (ppMany scr)
        withCurrentIndent $ \i ->
            return ((i, header) : script_pp'd)
compoundMessage _ stmt = preStatement $ stmt

-- RHS is a localizable atom.
withLocAtom :: (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtom msg (Statement _ rhs) | Just key <- textRhs rhs
    = msgToPP =<< msg <$> getGameL10n key

-- RHS is a localizable atom and we need a second one (passed to message as
-- first arg).
withLocAtom2 :: ScriptMessage -> (Text -> Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtom2 inMsg msg (Statement _ rhs) | Just key <- textRhs rhs
    = msgToPP =<< msg <$> messageText inMsg <*> getGameL10n key
withLocAtom2 inMsg msg stmt@(Statement _ rhs) = plainMsg $ pre_statement' stmt

withLocAtomAndIcon :: Text -> (Text -> Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtomAndIcon iconkey msg (Statement _ rhs) | Just key <- textRhs rhs
    = do what <- getGameL10n key
         msgToPP $ msg (iconText iconkey) what

withLocAtomIcon :: (Text -> Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtomIcon msg stmt@(Statement _ rhs) | Just key <- textRhs rhs
    = withLocAtomAndIcon key msg stmt

withProvince :: (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withProvince msg (Statement lhs rhs) =
    let loc_key = "PROV" <> case rhs of
            IntRhs id -> show id
            -- Province IDs shouldn't parse as float, but unfortunately they
            -- do. Just ignore the fractional part.
            -- NB: floor might give us an ID 1 too low, so round instead.
            FloatRhs id -> show (round id)
    in withLocAtom msg (Statement lhs (GenericRhs (T.pack loc_key)))

-- As withLocAtom but no l10n.
-- Currently unused
--withNonlocAtom :: (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
--withNonlocAtom msg (Statement _ rhs) | Just text <- textRhs rhs
--    = msgToPP $ msg text
--withNonlocAtom _ stmt = preStatement stmt

-- As withNonlocAtom but with an additional bit of text.
withNonlocAtom2 :: ScriptMessage -> (Text -> Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withNonlocAtom2 submsg msg (Statement _ rhs) | Just text <- textRhs rhs = do
    extratext <- messageText submsg
    msgToPP $ msg extratext text
withNonlocAtom2 _ _ stmt = preStatement stmt

-- Table of script atom -> icon key. Only ones that are different are listed.
scriptIconTable :: HashMap Text Text
scriptIconTable = HM.fromList
    [("master_of_mint", "master of mint")
    ,("natural_scientist", "natural scientist")
    ,("colonial_governor", "colonial governor")
    ,("diplomat", "diplomat_adv")
    ,("naval_reformer", "naval reformer")
    ,("army_organizer", "army organizer")
    ,("army_reformer", "army reformer")
    ,("grand_captain", "grand captain")
    ,("master_recruiter", "master recruiter")
    ,("military_engineer", "military engineer")
    ,("spy_ideas", "espionage")
    ,("estate_church", "clergy")
    ,("estate_nobles", "nobles")
    ,("estate_burghers", "burghers")
    ,("estate_cossacks", "cossacks")
    ,("estate_nomadic_tribes", "tribes")
    ,("estate_dhimmi", "dhimmi")
    ,("base production", "production")
    ,("particularist", "particularists")
    ,("is_monarch_leader", "ruler general")
    ]

-- As generic_icon except
-- * say "same as <foo>" if foo refers to a country (in which case, add a flag)
-- * may not actually have an icon (localization file will know if it doesn't)
iconOrFlag :: (Text -> Text -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
iconOrFlag iconmsg flagmsg (Statement (GenericLhs category) (GenericRhs name)) = msgToPP =<< do
    nflag <- flag name -- laziness means this might not get evaluated
    if isTag name || isPronoun name
        then return . flagmsg . doc2text $ nflag
        else iconmsg <$> return (iconText . HM.lookupDefault name name $ scriptIconTable)
                     <*> getGameL10n name
iconOrFlag _ _ stmt = plainMsg $ pre_statement' stmt

tagOrProvince :: (Text -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
tagOrProvince tagmsg provmsg (Statement _ rhs)
    = let eobject = case rhs of
            GenericRhs tag -> Left tag
            IntRhs provid -> Right provid
            FloatRhs provid -> Right (round provid)
      in msgToPP =<< case eobject of
            Left tag -> do -- is a tag
                tagflag <- flag tag
                return . tagmsg . doc2text $ tagflag
            Right provid -> do -- is a province id
                prov_loc <- getProvLoc provid
                return . provmsg $ prov_loc

-- Numeric statement.
numeric :: (Double -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
numeric msg (Statement _ rhs) 
    | Just n <- floatRhs rhs = msgToPP $ msg n
numeric _ stmt = plainMsg $ pre_statement' stmt

numericOrTag :: (Double -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
numericOrTag numMsg tagMsg (Statement _ rhs) = msgToPP =<<
    case rhs of
        IntRhs n -> return $ numMsg (fromIntegral n)
        FloatRhs n -> return $ numMsg n
        GenericRhs t -> do -- assume it's a tag
            tflag <- flag t
            return $ tagMsg (doc2text tflag)

-- Generic statement referring to a country. Use a flag.
withFlag :: (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withFlag msg (Statement _ (GenericRhs who)) = msgToPP =<< do
    whoflag <- flag who
    return . msg . doc2text $ whoflag
withFlag _ stmt = plainMsg $ pre_statement' stmt

withBool :: (Bool -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (plainMsg $ pre_statement' stmt)
          return
          fullmsg

withBool' :: (Bool -> ScriptMessage) -> GenericStatement -> PP extra (Maybe [(Int, ScriptMessage)])
withBool' msg (Statement _ rhs) | Just yn <- textRhs rhs, T.map toLower yn `elem` ["yes","no"]
    = fmap Just . msgToPP $ case T.toCaseFold yn of
        "yes" -> msg True
        "no"  -> msg False
        _     -> error "impossible: withBool matched a string that wasn't yes or no"
withBool' _ stmt = return Nothing

-- Statement may have "yes"/"no" or a tag.
withFlagOrBool :: (Bool -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withFlagOrBool bmsg _ (Statement _ (GenericRhs "yes")) = msgToPP (bmsg True)
withFlagOrBool bmsg _ (Statement _ (GenericRhs "no"))  = msgToPP (bmsg False)
withFlagOrBool _ tmsg stmt = withFlag tmsg stmt

numericIcon :: Text -> (Text -> Double -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
numericIcon the_icon msg (Statement _ rhs) | Just amt <- floatRhs rhs
    = msgToPP $ msg (iconText the_icon) amt
numericIcon _ _ stmt = plainMsg $ pre_statement' stmt

-- LHS is a localizable atom and has an icon.
numericIcon2 :: (Text -> Text -> Double -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
numericIcon2 msg (Statement (GenericLhs lhs) rhs) | Just amt <- floatRhs rhs = do
    lhs_loc <- getGameL10n lhs
    msgToPP $ msg (iconText lhs) lhs_loc amt

numLhsMsg :: Text -> Maybe (Text -> Text -> Double -> ScriptMessage)
numLhsMsg "orthodox" = Just MsgHasReligionProvinces
numLhsMsg _ = Nothing

---------------------------------
-- Specific statement handlers --
---------------------------------

data FactionInfluence = FactionInfluence {
        faction :: Maybe Text
    ,   influence :: Maybe Double
    }
newInfluence = FactionInfluence Nothing Nothing
factionInfluence :: GenericStatement -> PP extra [(Int, ScriptMessage)]
factionInfluence stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_influence $ foldl' addField newInfluence scr)
    where
        pp_influence inf =
            if isJust (faction inf) && isJust (influence inf)
            then
                let fac = fromJust (faction inf)
                    fac_iconkey = case fac of
                            -- Celestial empire
                            "enuchs" {- sic -} -> "eunuchs influence"
                            "temples"          -> "temples influence"
                            "bureaucrats"      -> "bureaucrats influence"
                            -- Merchant republic
                            "mr_aristocrats"   -> "aristocrats influence"
                            "mr_guilds"        -> "guilds influence"
                            "mr_traders"       -> "traders influence"
                    fac_icon = iconText fac_iconkey
                    infl = fromJust (influence inf)
                in do
                    fac_loc <- getGameL10n fac
                    return $ MsgFactionGainInfluence fac_icon fac_loc infl
            else return $ preMessage stmt
        addField :: FactionInfluence -> GenericStatement -> FactionInfluence
        addField inf (Statement (GenericLhs "faction") (GenericRhs fac)) = inf { faction = Just fac }
        addField inf (Statement (GenericLhs "influence") (FloatRhs amt)) = inf { influence = Just amt }
        addField inf (Statement (GenericLhs "influence") (IntRhs amt)) = inf { influence = Just (fromIntegral amt) }
        addField inf _ = inf -- unknown statement

gainIcon :: Bool -> Text
            -> (Text -> Double -> ScriptMessage)
            -> GenericStatement
            -> PP extra [(Int, ScriptMessage)]
gainIcon good iconkey msg
    (Statement _ rhs) | Just n <- floatRhs rhs
    = msgToPP $ msg (iconText iconkey) n

gain' :: Bool -> (Double -> ScriptMessage)
              -> GenericStatement
              -> PP extra [(Int, ScriptMessage)]
gain' good msg
    (Statement _ rhs) | Just n <- floatRhs rhs
    = msgToPP $ msg n

data Modifier = Modifier {
        mod_name :: Maybe Text
    ,   mod_key :: Maybe Text
    ,   mod_who :: Maybe Text
    ,   mod_duration :: Maybe Double
    ,   mod_power :: Maybe Double
    } deriving Show
newModifier = Modifier Nothing Nothing Nothing Nothing Nothing

addModifierLine :: Modifier -> GenericStatement -> Modifier 
addModifierLine apm (Statement (GenericLhs "name") (GenericRhs name)) = apm { mod_name = Just name }
addModifierLine apm (Statement (GenericLhs "name") (StringRhs name)) = apm { mod_name = Just name }
addModifierLine apm (Statement (GenericLhs "key") (GenericRhs key)) = apm { mod_key = Just key }
addModifierLine apm (Statement (GenericLhs "key") (StringRhs key)) = apm { mod_key = Just key }
addModifierLine apm (Statement (GenericLhs "who") (GenericRhs tag)) = apm { mod_who = Just tag }
addModifierLine apm (Statement (GenericLhs "who") (StringRhs tag)) = apm { mod_who = Just tag }
addModifierLine apm (Statement (GenericLhs "duration") (FloatRhs duration)) = apm { mod_duration = Just duration }
addModifierLine apm (Statement (GenericLhs "power") (FloatRhs power)) = apm { mod_power = Just power }
addModifierLine apm _ = apm -- e.g. hidden = yes

maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f x = maybe (return Nothing) (liftM Just . f) x

addModifier :: ScriptMessage -> GenericStatement -> PP extra [(Int, ScriptMessage)]
addModifier kind stmt@(Statement _ (CompoundRhs scr)) = msgToPP =<<
    let mod = foldl' addModifierLine newModifier scr
    in if isJust (mod_name mod) || isJust (mod_key mod) then do
        kind <- messageText kind
        mwho <- maybe (return Nothing) (fmap (Just . doc2text) . flag) (mod_who mod)
        mname_loc <- maybeM getGameL10n (mod_name mod)
        mkey_loc <- maybeM getGameL10n (mod_key mod)
        let mdur = mod_duration mod

        case (mwho, mname_loc, mkey_loc, mod_power mod, mdur) of
            (_, Nothing, Nothing, _, _) -> return (preMessage stmt) -- need a name for the mod!
            (Nothing,  Just name, _,        Nothing,  Nothing)  -> return (MsgGainMod kind name)
            (Nothing,  Nothing,   Just key, Nothing,  Nothing)  -> return (MsgGainMod kind key)
            (Nothing,  Just name, _,        Nothing,  Just dur) -> return (MsgGainModDur kind name dur)
            (Nothing,  Nothing,   Just key, Nothing,  Just dur) -> return (MsgGainModDur kind key dur)
            (Nothing,  Just name, _,        Just pow, Nothing)  -> return (MsgGainModPow kind name pow)
            (Nothing,  Nothing,   Just key, Just pow, Nothing)  -> return (MsgGainModPow kind key pow)
            (Nothing,  Just name, _,        Just pow, Just dur) -> return (MsgGainModPowDur kind name pow dur)
            (Nothing,  Nothing,   Just key, Just pow, Just dur) -> return (MsgGainModPowDur kind key pow dur)
            (Just who, Just name, _,        Nothing,  Nothing)  -> return (MsgActorGainsMod kind who name)
            (Just who, Nothing,   Just key, Nothing,  Nothing)  -> return (MsgActorGainsMod kind who key)
            (Just who, Just name, _,        Nothing,  Just dur) -> return (MsgActorGainsModDur kind who name dur)
            (Just who, Nothing,   Just key, Nothing,  Just dur) -> return (MsgActorGainsModDur kind who key dur)
            (Just who, Just name, _,        Just pow, Nothing)  -> return (MsgActorGainsModPow kind who name pow)
            (Just who, Nothing,   Just key, Just pow, Nothing)  -> return (MsgActorGainsModPow kind who key pow)
            (Just who, Just name, _,        Just pow, Just dur) -> return (MsgActorGainsModPowDur kind who name pow dur)
            (Just who, Nothing,   Just key, Just pow, Just dur) -> return (MsgActorGainsModPowDur kind who key pow dur)
    else return (preMessage stmt)
addModifier _ stmt = preStatement stmt

-- "add_core = <n>" in country scope means "Gain core on <localize PROVn>"
-- "add_core = <tag>" in province scope means "<localize tag> gains core"
addCore :: GenericStatement -> PP extra [(Int, ScriptMessage)]
addCore (Statement _ (GenericRhs tag)) = msgToPP =<< do -- tag
    tagflag <- flagText tag
    return $ MsgTagGainsCore tagflag
addCore (Statement _ rhs) | Just num <- floatRhs rhs = msgToPP =<< do -- province
    prov <- getProvLoc num
    return $ MsgGainCoreOnProvince prov
addCore stmt = preStatement stmt

-- Add an opinion modifier towards someone (for a number of years).
data AddOpinion = AddOpinion {
        who :: Maybe Text
    ,   modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show
newAddOpinion = AddOpinion Nothing Nothing Nothing

opinion :: (Text -> Text -> ScriptMessage)
        -> (Text -> Text -> Double -> ScriptMessage)
        -> GenericStatement -> PP extra [(Int, ScriptMessage)]
opinion msgIndef msgDur stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_add_opinion $ foldl' addLine newAddOpinion scr)
    where
        addLine :: AddOpinion -> GenericStatement -> AddOpinion
        addLine op (Statement (GenericLhs "who") (GenericRhs tag))
            = op { who = Just tag }
        addLine op (Statement (GenericLhs "modifier") (GenericRhs label))
            = op { modifier = Just label }
        addLine op (Statement (GenericLhs "years") rhs) | Just n <- floatRhs rhs
            = op { op_years = Just n }
        addLine op _ = op
        pp_add_opinion op
            = if isJust (who op) && isJust (modifier op) then do
                let whom = fromJust (who op)
                whomflag <- doc2text <$> flag whom
                mod_loc <- getGameL10n (fromJust (modifier op))
                if isNothing (op_years op)
                    then return $ msgIndef mod_loc whomflag
                    else return $ msgDur mod_loc whomflag (fromJust (op_years op))
              else return (preMessage stmt)
opinion _ _ stmt = preStatement stmt

-- Render a rebel type atom (e.g. anti_tax_rebels) as their name and icon key.
-- This is needed because all religious rebels localize as simply "Religious" -
-- we want to be more specific.
rebel_loc :: HashMap Text (Text,Text)
rebel_loc = HM.fromList
        [("polish_noble_rebels",    ("Magnates", "magnates"))
        ,("lollard_rebels",         ("Lollard zealots", "lollards"))
        ,("catholic_rebels",        ("Catholic zealots", "catholic zealots"))
        ,("protestant_rebels",      ("Protestant zealots", "protestant zealots"))
        ,("reformed_rebels",        ("Reformed zealots", "reformed zealots"))
        ,("orthodox_rebels",        ("Orthodox zealots", "orthodox zealots"))
        ,("sunni_rebels",           ("Sunni zealots", "sunni zealots"))
        ,("shiite_rebels",          ("Shiite zealots", "shiite zealots"))
        ,("buddhism_rebels",        ("Buddhist zealots", "buddhist zealots"))
        ,("mahayana_rebels",        ("Mahayana zealots", "mahayana zealots"))
        ,("vajrayana_rebels",       ("Vajrayana zealots", "vajrayana zealots"))
        ,("hinduism_rebels",        ("Hindu zealots", "hindu zealots"))
        ,("confucianism_rebels",    ("Confucian zealots", "confucian zealots"))
        ,("shinto_rebels",          ("Shinto zealots", "shinto zealots"))
        ,("animism_rebels",         ("Animist zealots", "animist zealots"))
        ,("shamanism_rebels",       ("Shamanist zealots", "shamanist zealots"))
        ,("totemism_rebels",        ("Totemist zealots", "totemist zealots"))
        ,("coptic_rebels",          ("Coptic zealots", "coptic zealots"))
        ,("ibadi_rebels",           ("Ibadi zealots", "ibadi zealots"))
        ,("sikhism_rebels",         ("Sikh zealots", "sikh zealots"))
        ,("jewish_rebels",          ("Jewish zealots", "jewish zealots"))
        ,("norse_pagan_reformed_rebels", ("Norse zealots", "norse zealots"))
        ,("inti_rebels",            ("Inti zealots", "inti zealots"))
        ,("maya_rebels",            ("Maya zealots", "maya zealots"))
        ,("nahuatl_rebels",         ("Nahuatl zealots", "nahuatl zealots"))
        ,("tengri_pagan_reformed_rebels", ("Tengri zealots", "tengri zealots"))
        ,("zoroastrian_rebels",     ("Zoroastrian zealots", "zoroastrian zealots"))
        ,("ikko_ikki_rebels",       ("Ikko-Ikkis", "ikko-ikkis"))
        ,("ronin_rebels",           ("Ronin rebels", "ronin"))
        ,("reactionary_rebels",     ("Reactionaries", "reactionaries"))
        ,("anti_tax_rebels",        ("Peasant rabble", "peasants"))
        ,("revolutionary_rebels",   ("Revolutionaries", "revolutionaries"))
        ,("heretic_rebels",         ("Heretics", "heretics"))
        ,("religious_rebels",       ("Religious zealots", "religious zealots"))
        ,("nationalist_rebels",     ("Separatist rebels", "separatists"))
        ,("noble_rebels",           ("Noble rebels", "noble rebels"))
        ,("colonial_rebels",        ("Colonial rebels", "colonial rebels")) -- ??
        ,("patriot_rebels",         ("Patriot rebels", "patriot"))
        ,("pretender_rebels",       ("Pretender rebels", "pretender"))
        ,("colonial_patriot_rebels", ("Colonial patriot", "colonial patriot")) -- ??
        ,("particularist_rebels",   ("Particularist rebels", "particularist"))
        ,("nationalist_rebels",   ("Nationalist rebels", "separatists"))
        ]

-- Spawn a rebel stack.
data SpawnRebels = SpawnRebels {
        rebelType :: Maybe Text
    ,   rebelSize :: Maybe Double
    ,   friend :: Maybe Text
    ,   win :: Bool
    ,   sr_unrest :: Maybe Double -- rebel faction progress
    ,   sr_leader :: Maybe Text
    } deriving Show
newSpawnRebels = SpawnRebels Nothing Nothing Nothing False Nothing Nothing

spawnRebels :: Maybe Text -> GenericStatement -> PP extra [(Int, ScriptMessage)]
spawnRebels mtype stmt = msgToPP =<< spawnRebels' mtype stmt where
    spawnRebels' Nothing stmt@(Statement _ (CompoundRhs scr))
        = pp_spawnRebels $ foldl' addLine newSpawnRebels scr
    spawnRebels' rtype stmt@(Statement _ rhs) | Just size <- floatRhs rhs
        = pp_spawnRebels $ newSpawnRebels { rebelType = rtype, rebelSize = Just size }

    addLine :: SpawnRebels -> GenericStatement -> SpawnRebels
    addLine op (Statement (GenericLhs "type") (GenericRhs tag))
        = op { rebelType = Just tag }
    addLine op (Statement (GenericLhs "size") (FloatRhs n))
        = op { rebelSize = Just n }
    addLine op (Statement (GenericLhs "friend") (GenericRhs tag))
        = op { friend = Just tag }
    addLine op (Statement (GenericLhs "win") (GenericRhs "yes"))
        = op { win = True }
    addLine op (Statement (GenericLhs "unrest") (FloatRhs n))
        = op { sr_unrest = Just n }
    addLine op (Statement (GenericLhs "leader") (StringRhs name))
        = op { sr_leader = Just name }
    addLine op _ = op

    pp_spawnRebels :: SpawnRebels -> PP extra ScriptMessage
    pp_spawnRebels reb
        = if isJust (rebelSize reb) then do
            let rtype_loc_icon = flip HM.lookup rebel_loc =<< rebelType reb
            friendText <- case friend reb of
                Just friend -> do
                    flag <- flagText friend
                    text <- messageText (MsgRebelsFriendlyTo flag)
                    return (" (" <> text <> ")")
                Nothing -> return ""
            leaderText <- case sr_leader reb of
                Just leader -> do
                    text <- messageText (MsgRebelsLedBy leader)
                    return (" (" <> text <> ")")
                Nothing -> return ""
            progressText <- case sr_unrest reb of
                Just unrest -> do
                    text <- messageText (MsgRebelsGainProgress unrest)
                    return (" (" <> text <> ")")
                Nothing -> return ""
            return $ MsgSpawnRebels
                        (maybe "" (\(ty, ty_icon) -> iconText ty_icon <> " " <> ty) rtype_loc_icon)
                        (fromJust (rebelSize reb))
                        friendText
                        leaderText
                        (win reb)
                        progressText
        else return $ preMessage stmt

hasSpawnedRebels :: GenericStatement -> PP extra [(Int, ScriptMessage)]
hasSpawnedRebels (Statement _ (GenericRhs rtype))
    | Just (rtype_loc, rtype_iconkey) <- HM.lookup rtype rebel_loc
      = msgToPP $ MsgRebelsHaveRisen (iconText rtype_iconkey) rtype_loc
hasSpawnedRebels stmt = preStatement stmt

canSpawnRebels :: GenericStatement -> PP extra [(Int, ScriptMessage)]
canSpawnRebels (Statement _ (GenericRhs rtype))
    | Just (rtype_loc, rtype_iconkey) <- HM.lookup rtype rebel_loc
      = msgToPP (MsgProvinceHasRebels (iconText rtype_iconkey) rtype_loc)
canSpawnRebels stmt = preStatement stmt

data TriggerEvent = TriggerEvent
        { e_id :: Maybe Text
        , e_title_loc :: Maybe Text
        , e_days :: Maybe Double
        }
newTriggerEvent = TriggerEvent Nothing Nothing Nothing
triggerEvent :: ScriptMessage -> GenericStatement -> PP extra [(Int, ScriptMessage)]
triggerEvent evtType stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< pp_trigger_event =<< foldM addLine newTriggerEvent scr
    where
        addLine :: TriggerEvent -> GenericStatement -> PP extra TriggerEvent
        addLine evt (Statement (GenericLhs "id") (GenericRhs id))
            = (\t_loc -> evt { e_id = Just id, e_title_loc = t_loc })
              <$> getGameL10nIfPresent (id <> ".t")
        addLine evt (Statement (GenericLhs "days") rhs) = return $ case rhs of
            IntRhs n -> evt { e_days = Just (fromIntegral n) }
            FloatRhs n -> evt { e_days = Just n }
        addLine evt _ = return evt
        pp_trigger_event :: TriggerEvent -> PP extra ScriptMessage
        pp_trigger_event evt = do
            let mloc = e_title_loc evt
                mid = e_id evt
                id = fromJust mid
                loc = if isJust mloc then fromJust mloc else id
                mdays = e_days evt
                days = fromJust mdays
            evtType_t <- messageText evtType
            if isJust mid then let msgid = fromJust mid in
                if isJust mdays then
                    return $ MsgTriggerEventDays evtType_t msgid loc days
                else
                    return $ MsgTriggerEvent evtType_t msgid loc
            else return $ preMessage stmt

gainManpower :: GenericStatement -> PP extra [(Int, ScriptMessage)]
gainManpower (Statement _ rhs) | Just amt <- floatRhs rhs = msgToPP =<<
    let mpicon = iconText "manpower"
    in if abs (amt::Double) < 1
        --  interpret amt as a fraction of max
        then return $ MsgGainMPFrac mpicon amt
        --  interpret amt as a multiple of 1,000
        else return $ MsgGainMP mpicon (amt*1000)


data AddCB = AddCB
    {   acb_target :: Maybe Text
    ,   acb_target_flag :: Maybe Text
    ,   acb_type :: Maybe Text
    ,   acb_type_loc :: Maybe Text
    ,   acb_months :: Maybe Double
    }
newAddCB = AddCB Nothing Nothing Nothing Nothing Nothing
-- "direct" is False for reverse_add_casus_belli
addCB :: Bool -> GenericStatement -> PP extra [(Int, ScriptMessage)]
addCB direct stmt@(Statement _ (CompoundRhs scr))
    = msgToPP . pp_add_cb =<< foldM addLine newAddCB scr where
        addLine :: AddCB -> GenericStatement -> PP extra AddCB
        addLine acb (Statement (GenericLhs "target") (GenericRhs target))
            = (\target_loc -> acb
                  { acb_target = Just target
                  , acb_target_flag = Just (doc2text target_loc) })
              <$> flag target
        addLine acb (Statement (GenericLhs "type") (GenericRhs cbtype))
            = (\cbtype_loc -> acb
                  { acb_type = Just cbtype
                  , acb_type_loc = cbtype_loc })
              <$> getGameL10nIfPresent cbtype
        addLine acb (Statement (GenericLhs "months") rhs)
            = return $ acb { acb_months = Just months }
            where months = case rhs of
                    IntRhs n -> fromIntegral n
                    FloatRhs n -> n
        pp_add_cb :: AddCB -> ScriptMessage
        pp_add_cb acb =
            case (acb_type acb, acb_type_loc acb,
                  acb_target_flag acb,
                  acb_months acb) of
                (Nothing, _, _, _) -> preMessage stmt -- need CB type
                (_, _, Nothing, _) -> preMessage stmt -- need target
                (_, Just cbtype_loc, Just target_flag, Just months) -> MsgGainCBDuration cbtype_loc target_flag months
                (Just cbtype, Nothing, Just target_flag, Just months) -> MsgGainCBDuration cbtype target_flag months
                (_, Just cbtype_loc, Just target_flag, Nothing) -> MsgGainCB cbtype_loc target_flag
                (Just cbtype, Nothing, Just target_flag, Nothing) -> MsgGainCB cbtype target_flag

random :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
random stmt@(Statement _ (CompoundRhs scr))
    | (front, back) <- break
                        (\stmt -> case stmt of 
                            Statement (GenericLhs "chance") _ -> True
                            _ -> False)
                        scr
      , not (null back) =
        let chance = case head back of
                Statement _ (IntRhs n) -> fromIntegral n
                Statement _ (FloatRhs n) -> n
        in compoundMessage
                (MsgRandomChance chance)
                (Statement undefined (CompoundRhs (front ++ tail back)))
    | otherwise = compoundMessage MsgRandom stmt
random stmt = preStatement stmt

data DefineAdvisor = DefineAdvisor
    {   da_type :: Maybe Text
    ,   da_type_loc :: Maybe Text
    ,   da_name :: Maybe Text
    ,   da_discount :: Maybe Bool
    ,   da_location :: Maybe Int
    ,   da_location_loc :: Maybe Text
    ,   da_skill :: Maybe Double
    ,   da_female :: Maybe Bool
    }
newDefineAdvisor = DefineAdvisor Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

defineAdvisor :: GenericStatement -> PP extra [(Int, ScriptMessage)]
defineAdvisor stmt@(Statement _ (CompoundRhs scr))
    = msgToPP . pp_define_advisor =<< foldM addLine newDefineAdvisor scr where
        addLine :: DefineAdvisor -> GenericStatement -> PP extra DefineAdvisor
        addLine da stmt@(Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
            "type" ->
                let mthe_type = case rhs of
                        GenericRhs a_type -> Just a_type
                        StringRhs a_type -> Just a_type
                        _ -> Nothing
                in (\mtype_loc -> da
                        { da_type = mthe_type
                        , da_type_loc = mtype_loc })
                   <$> (maybe (return Nothing) getGameL10nIfPresent mthe_type)
            "name" -> return $
                let mthe_name = case rhs of
                        GenericRhs a_name -> Just a_name
                        StringRhs a_name -> Just a_name
                        _ -> Nothing
                in da { da_name = mthe_name }
            "discount" -> return $
                let yn = case rhs of
                        GenericRhs yn' -> Just yn'
                        StringRhs yn' -> Just yn'
                        _ -> Nothing
                in if yn == Just "yes" then da { da_discount = Just True }
                   else if yn == Just "no" then da { da_discount = Just False }
                   else da
            "location" -> do
                let location_code = floatRhs rhs
                location_loc <- sequence (getProvLoc <$> location_code)
                return $ da { da_location = location_code
                            , da_location_loc = location_loc }
            "skill" -> return $ da { da_skill = floatRhs rhs }
            "female" -> return $
                let yn = case rhs of
                        GenericRhs yn' -> Just yn'
                        StringRhs yn' -> Just yn'
                        _ -> Nothing
                in if yn == Just "yes" then da { da_female = Just True }
                   else if yn == Just "no" then da { da_female = Just False }
                   else da
        pp_define_advisor :: DefineAdvisor -> ScriptMessage
        pp_define_advisor da =
            let mskill = da_skill da
            in if isJust mskill then
                let skill = fromJust mskill
                    mdiscount = da_discount da
                    discount = isJust mdiscount && fromJust mdiscount
                    mlocation_loc = da_location_loc da
                    mlocation = maybe (T.pack . show <$> da_location da) Just mlocation_loc
                in case (da_female da,
                           da_type da,
                           da_name da,
                           mlocation) of
                    (Nothing, Nothing, Nothing, Nothing)
                        -> (if discount then MsgGainAdvisorDiscount else MsgGainAdvisor) skill
                    (Nothing, Nothing, Nothing, Just location)
                        -> (if discount then MsgGainAdvisorLocDiscount else MsgGainAdvisorLoc)
                            location skill
                    (Nothing, Nothing, Just name, Nothing)
                        -> (if discount then MsgGainAdvisorNameDiscount else MsgGainAdvisorName)
                            name skill
                    (Nothing, Nothing, Just name, Just location)
                        -> (if discount then MsgGainAdvisorNameLocDiscount else MsgGainAdvisorNameLoc)
                            name location skill
                    (Nothing, Just advtype, Nothing, Nothing)
                        -> (if discount then MsgGainAdvisorTypeDiscount else MsgGainAdvisorType)
                            advtype skill
                    (Nothing, Just advtype, Nothing, Just location)
                        -> (if discount then MsgGainAdvisorTypeLocDiscount else MsgGainAdvisorTypeLoc)
                            advtype location skill
                    (Nothing, Just advtype, Just name, Nothing)
                        -> (if discount then MsgGainAdvisorTypeNameDiscount else MsgGainAdvisorTypeName)
                            advtype name skill
                    (Nothing, Just advtype, Just name, Just location)
                        -> (if discount then MsgGainAdvisorTypeNameLocDiscount else MsgGainAdvisorTypeNameLoc)
                            advtype name location skill
                    (Just female, Nothing, Nothing, Nothing)
                        -> (if discount then MsgGainFemaleAdvisorDiscount else MsgGainFemaleAdvisor)
                            female skill
                    (Just female, Nothing, Nothing, Just location)
                        -> (if discount then MsgGainFemaleAdvisorLocDiscount else MsgGainFemaleAdvisorLoc)
                            female location skill
                    (Just female, Nothing, Just name, Nothing)
                        -> (if discount then MsgGainFemaleAdvisorNameDiscount else MsgGainFemaleAdvisorName)
                            female name skill
                    (Just female, Nothing, Just name, Just location)
                        -> (if discount then MsgGainFemaleAdvisorNameLocDiscount else MsgGainFemaleAdvisorNameLoc)
                            female name location skill
                    (Just female, Just advtype, Nothing, Nothing)
                        -> (if discount then MsgGainFemaleAdvisorTypeDiscount else MsgGainFemaleAdvisorType)
                            female advtype skill
                    (Just female, Just advtype, Nothing, Just location)
                        -> (if discount then MsgGainFemaleAdvisorTypeLocDiscount else MsgGainFemaleAdvisorTypeLoc)
                            female advtype location skill
                    (Just female, Just advtype, Just name, Nothing)
                        -> (if discount then MsgGainFemaleAdvisorTypeNameDiscount else MsgGainFemaleAdvisorTypeName)
                            female advtype name skill
                    (Just female, Just advtype, Just name, Just location)
                        -> (if discount then MsgGainFemaleAdvisorTypeNameLocDiscount else MsgGainFemaleAdvisorTypeNameLoc)
                            female advtype name location skill
             else preMessage stmt

data DefineRuler = DefineRuler
    {   dr_rebel :: Bool
    ,   dr_name :: Maybe Text
    ,   dr_dynasty :: Maybe Text -- can be a tag/pronoun
    ,   dr_age :: Maybe Double
    ,   dr_female :: Maybe Bool
    ,   dr_claim :: Maybe Double
    ,   dr_regency :: Bool
    ,   dr_adm :: Maybe Int
    ,   dr_dip :: Maybe Int
    ,   dr_mil :: Maybe Int
    ,   dr_fixed :: Bool
    ,   dr_attach_leader :: Maybe Text
    }
newDefineRuler = DefineRuler False Nothing Nothing Nothing Nothing Nothing False Nothing Nothing Nothing False Nothing

defineRuler :: GenericStatement -> PP extra [(Int, ScriptMessage)]
defineRuler stmt@(Statement _ (CompoundRhs scr))
    = pp_define_ruler $ foldl' addLine newDefineRuler scr where
        addLine :: DefineRuler -> GenericStatement -> DefineRuler
        addLine dr stmt@(Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
            "rebel" -> case rhs of
                GenericRhs "yes" -> dr { dr_rebel = True }
                _ -> dr
            "name" ->
                let mthe_name = case rhs of
                        GenericRhs a_name -> Just a_name
                        StringRhs a_name -> Just a_name
                        _ -> Nothing
                in dr { dr_name = mthe_name }
            "dynasty" ->
                let mthe_dynasty = case rhs of
                        GenericRhs a_dynasty -> Just a_dynasty
                        StringRhs a_dynasty -> Just a_dynasty
                        _ -> Nothing
                in dr { dr_dynasty = mthe_dynasty }
            "age" ->
                let mage = floatRhs rhs
                in  dr { dr_age = mage }
            "female" ->
                let yn = case rhs of
                        GenericRhs yn' -> Just yn'
                        StringRhs yn' -> Just yn'
                        _ -> Nothing
                in if yn == Just "yes" then dr { dr_female = Just True }
                   else if yn == Just "no" then dr { dr_female = Just False }
                   else dr
            "claim" ->
                let mclaim = floatRhs rhs
                in  dr { dr_claim = mclaim }
            "regency" -> case textRhs rhs of
                Just "yes" -> dr { dr_regency = True }
                _ -> dr
            "adm" ->
                let madm = floatRhs rhs
                in  dr { dr_adm = madm }
            "dip" ->
                let mdip = floatRhs rhs
                in  dr { dr_dip = mdip }
            "mil" ->
                let mmil = floatRhs rhs
                in  dr { dr_mil = mmil }
            "fixed" -> case rhs of
                GenericRhs "yes" -> dr { dr_fixed = True }
                _ -> dr
            "attach_leader" -> dr { dr_attach_leader = textRhs rhs }
        pp_define_ruler :: DefineRuler -> PP extra [(Int, ScriptMessage)]
        pp_define_ruler dr@DefineRuler { dr_rebel = True } = msgToPP MsgRebelLeaderRuler
        pp_define_ruler dr@DefineRuler { dr_regency = regency, dr_attach_leader = mleader } = do
            body <- indentUp (unfoldM pp_define_ruler_attrib dr)
            if null body
                then msgToPP (MsgNewRuler regency)
                else do
                    heading <- msgToPP (MsgNewRulerAttribs regency)
                    return (heading ++ body)
        pp_define_ruler_attrib :: DefineRuler -> PP extra (Maybe ((Int, ScriptMessage), DefineRuler))
        -- "Named <foo>"
        pp_define_ruler_attrib dr@DefineRuler { dr_name = Just name } = do
            [msg] <- msgToPP (MsgNewRulerName name)
            return (Just (msg, dr { dr_name = Nothing }))
        -- "Of the <foo> dynasty"
        pp_define_ruler_attrib dr@DefineRuler { dr_dynasty = Just dynasty } = do
            [msg] <- msgToPP (MsgNewRulerDynasty dynasty)
            return (Just (msg, dr { dr_dynasty = Nothing }))
        -- "Aged <foo> years"
        pp_define_ruler_attrib dr@DefineRuler { dr_age = Just age } = do
            [msg] <- msgToPP (MsgNewRulerAge age)
            return (Just (msg, dr { dr_age = Nothing }))
        -- "With {{icon|adm}} <foo> administrative skill"
        pp_define_ruler_attrib dr@DefineRuler { dr_adm = Just adm, dr_fixed = fixed } = do
            [msg] <- msgToPP (MsgNewRulerAdm fixed (fromIntegral adm))
            return (Just (msg, dr { dr_adm = Nothing }))
        -- "With {{icon|adm}} <foo> diplomatic skill"
        pp_define_ruler_attrib dr@DefineRuler { dr_dip = Just dip, dr_fixed = fixed } = do
            [msg] <- msgToPP (MsgNewRulerDip fixed (fromIntegral dip))
            return (Just (msg, dr { dr_dip = Nothing }))
        -- "With {{icon|adm}} <foo> military skill"
        pp_define_ruler_attrib dr@DefineRuler { dr_mil = Just mil, dr_fixed = fixed } = do
            [msg] <- msgToPP (MsgNewRulerMil fixed (fromIntegral mil))
            return (Just (msg, dr { dr_mil = Nothing }))
        -- Nothing left
        pp_define_ruler_attrib _ = return Nothing
defineRuler stmt = preStatement stmt

data HadFlag = HadFlag
    {   hf_flag :: Maybe Text
    ,   hf_days :: Maybe Int
    }
newHadFlag = HadFlag Nothing Nothing

hadFlag :: ScriptMessage -> GenericStatement -> PP extra [(Int, ScriptMessage)]
hadFlag category stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_had_flag $ foldl' addLine newHadFlag scr) where
        addLine :: HadFlag -> GenericStatement -> HadFlag
        addLine dr stmt@(Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
            "flag" -> case rhs of
                GenericRhs flagname -> dr { hf_flag = Just flagname }
                StringRhs flagname -> dr { hf_flag = Just flagname }
                _ -> dr
            "days" -> dr { hf_days = floatRhs rhs }
            _ -> dr
        pp_had_flag :: HadFlag -> PP extra ScriptMessage
        pp_had_flag dr
            = if isJust (hf_flag dr) && isJust (hf_days dr)
              then do
                cat <- messageText category
                return (MsgHadFlag cat (fromJust (hf_flag dr)) (fromIntegral (fromJust (hf_days dr))))
              else return (preMessage stmt)

data BuildToForcelimit = BuildToForcelimit
    {   btf_infantry :: Maybe Double
    ,   btf_cavalry :: Maybe Double
    ,   btf_artillery :: Maybe Double
    ,   btf_heavy_ship :: Maybe Double
    ,   btf_light_ship :: Maybe Double
    ,   btf_galley :: Maybe Double
    ,   btf_transport :: Maybe Double
    }
newBuildToForcelimit = BuildToForcelimit Nothing Nothing Nothing Nothing Nothing Nothing Nothing

buildToForcelimit :: GenericStatement -> PP extra [(Int, ScriptMessage)]
buildToForcelimit stmt@(Statement _ (CompoundRhs scr))
    = msgToPP . pp_build_to_forcelimit $ foldl' addLine newBuildToForcelimit scr where
        addLine :: BuildToForcelimit -> GenericStatement -> BuildToForcelimit
        addLine dr stmt@(Statement (GenericLhs lhs) rhs)
            = let mhowmuch = floatRhs rhs
                  howmuch = fromJust mhowmuch
              in if isNothing mhowmuch
                 then dr
                 else case T.map toLower lhs of
                    "infantry"   -> dr { btf_infantry   = Just howmuch }
                    "cavalry"    -> dr { btf_cavalry    = Just howmuch }
                    "artillery"  -> dr { btf_artillery  = Just howmuch }
                    "heavy_ship" -> dr { btf_heavy_ship = Just howmuch }
                    "light_ship" -> dr { btf_light_ship = Just howmuch }
                    "galley"     -> dr { btf_galley     = Just howmuch }
                    "transport"  -> dr { btf_transport  = Just howmuch }
                    _ -> dr
        pp_build_to_forcelimit :: BuildToForcelimit -> ScriptMessage
        pp_build_to_forcelimit dr =
            let has_infantry = isJust (btf_infantry dr)
                has_cavalry = isJust (btf_cavalry dr)
                has_artillery = isJust (btf_artillery dr)
                has_heavy_ship = isJust (btf_heavy_ship dr)
                has_light_ship = isJust (btf_light_ship dr)
                has_galley = isJust (btf_galley dr)
                has_transport = isJust (btf_transport dr)
                infantry = maybe 0 id (btf_infantry dr)
                cavalry = maybe 0 id (btf_cavalry dr)
                artillery = maybe 0 id (btf_artillery dr)
                heavy_ship = maybe 0 id (btf_heavy_ship dr)
                light_ship = maybe 0 id (btf_light_ship dr)
                galley = maybe 0 id (btf_galley dr)
                transport = maybe 0 id (btf_transport dr)
                has_land = has_infantry || has_cavalry || has_artillery
                has_navy = has_heavy_ship || has_light_ship || has_galley || has_transport
            in  if has_land == has_navy then
                    -- Neither or both. Unlikely, not provided for
                    preMessage stmt
                else if has_land then let
                    infIcon = iconText "infantry"
                    cavIcon = iconText "cavalry"
                    artIcon = iconText "artillery"
                    in MsgBuildToForcelimitLand infIcon infantry
                                                cavIcon cavalry
                                                artIcon artillery
                else let -- has_navy == True
                    heavyIcon = iconText "heavy ship"
                    lightIcon = iconText "light ship"
                    gallIcon = iconText "galley"
                    transpIcon = iconText "transport"
                    in MsgBuildToForcelimitNavy heavyIcon heavy_ship
                                                lightIcon light_ship
                                                gallIcon galley
                                                transpIcon transport
buildToForcelimit stmt = preStatement stmt

data DeclareWarWithCB = DeclareWarWithCB
    {   dwcb_who :: Maybe Text
    ,   dwcb_cb :: Maybe Text
    }
newDeclareWarWithCB = DeclareWarWithCB Nothing Nothing

declareWarWithCB :: GenericStatement -> PP extra [(Int, ScriptMessage)]
declareWarWithCB stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_declare_war_with_cb $ foldl' addLine newDeclareWarWithCB scr) where
        addLine :: DeclareWarWithCB -> GenericStatement -> DeclareWarWithCB
        addLine dwcb stmt@(Statement (GenericLhs lhs) (GenericRhs rhs))
            = case T.map toLower lhs of
                "who"         -> dwcb { dwcb_who = Just rhs }
                "casus_belli" -> dwcb { dwcb_cb  = Just rhs }
                _ -> dwcb
        pp_declare_war_with_cb :: DeclareWarWithCB -> PP extra ScriptMessage
        pp_declare_war_with_cb dwcb
            = let has_who = isJust (dwcb_who dwcb)
                  who = fromJust (dwcb_who dwcb)
                  has_cb = isJust (dwcb_cb dwcb)
                  cb = fromJust (dwcb_cb dwcb)
              in if has_who && has_cb
                 then do
                    whoflag <- doc2text <$> flag who
                    cb_loc <- getGameL10n cb
                    return (MsgDeclareWarWithCB whoflag cb_loc)
                 else return $ preMessage stmt
declareWarWithCB stmt = preStatement stmt

hasDlc :: GenericStatement -> PP extra [(Int, ScriptMessage)]
hasDlc (Statement _ (StringRhs dlc))
    = msgToPP $ MsgHasDLC dlc_icon dlc
    where
        mdlc_key = HM.lookup dlc . HM.fromList $
            [("Conquest of Paradise", "cop")
            ,("Wealth of Nations", "won")
            ,("Res Publica", "rp")
            ,("Art of War", "aow")
            ,("El Dorado", "ed")
            ,("Common Sense", "cs")
            ,("The Cossacks", "cos")
            ]
        dlc_icon = if isNothing mdlc_key then "" else iconText (fromJust mdlc_key)
hasDlc stmt = preStatement stmt

data EstateInfluenceModifier = EstateInfluenceModifier {
        eim_estate :: Maybe Text
    ,   eim_modifier :: Maybe Text
    }
newEIM = EstateInfluenceModifier Nothing Nothing
hasEstateInfluenceModifier :: GenericStatement -> PP extra [(Int, ScriptMessage)]
hasEstateInfluenceModifier stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< pp_eim (foldl' addField newEIM scr)
    where
        addField :: EstateInfluenceModifier -> GenericStatement -> EstateInfluenceModifier
        addField inf (Statement (GenericLhs "estate") (GenericRhs est)) = inf { eim_estate = Just est }
        addField inf (Statement (GenericLhs "modifier") (GenericRhs mod)) = inf { eim_modifier = Just mod }
        addField inf _ = inf -- unknown statement
        pp_eim inf = case (eim_estate inf, eim_modifier inf) of
            (Just est, Just mod) -> do
                loc_est <- getGameL10n est
                loc_mod <- getGameL10n mod
                return $ MsgEstateHasInfluenceModifier (iconText est) loc_est loc_mod
            _ -> return (preMessage stmt)
hasEstateInfluenceModifier stmt = preStatement stmt

triggerSwitch :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
-- A trigger switch must be of the form
-- trigger_switch = {
--  on_trigger = <statement lhs>
--  <statement rhs> = {
--      <actions>
--  }
-- }
-- where the <statement rhs> block may be repeated several times.
triggerSwitch stmt@(Statement _ (CompoundRhs
            -- Assume first line is on_trigger = <condlhs>
                        (Statement (GenericLhs "on_trigger") (GenericRhs condlhs)
                        :clauses))) = do
            statementsMsgs <- indentUp $ forM clauses $ \clause -> case clause of
                -- using next indent level, for each block <condrhs> = { ... }:
                Statement (GenericLhs condrhs) (CompoundRhs action) -> do
                    -- construct a fake condition to pp
                    let guard = Statement (GenericLhs condlhs) (GenericRhs condrhs)
                    ((_, guardMsg):_) <- ppOne guard -- XXX: match may fail (but shouldn't)
                    guardText <- messageText guardMsg
                    -- pp the rest of the block, at the next level
                    statementMsgs <- indentUp (ppMany action)
                    withCurrentIndent $ \i -> return $ (i, MsgTriggerSwitchClause guardText) : statementMsgs
                _ -> preStatement stmt
            withCurrentIndent $ \i -> return $ (i, MsgTriggerSwitch) : concat statementsMsgs
triggerSwitch stmt = preStatement stmt

data AddEstateInfluenceModifier = AddEstateInfluenceModifier {
        aeim_estate :: Maybe Text
    ,   aeim_desc :: Maybe Text
    ,   aeim_influence :: Maybe Double
    ,   aeim_duration :: Maybe Double
    } deriving Show
newAddEstateInfluenceModifier = AddEstateInfluenceModifier Nothing Nothing Nothing Nothing

timeOrIndef :: Double -> PP extra Text
timeOrIndef n = if n < 0 then messageText MsgIndefinitely else messageText (MsgForDays n)

estateInfluenceModifier :: (Text -> Text -> Text -> Double -> Text -> ScriptMessage)
                        -> GenericStatement -> PP extra [(Int, ScriptMessage)]
estateInfluenceModifier msg stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_eim $ foldl' addLine newAddEstateInfluenceModifier scr)
    where
        addLine :: AddEstateInfluenceModifier -> GenericStatement -> AddEstateInfluenceModifier 
        addLine aeim (Statement (GenericLhs "estate") (GenericRhs estate)) = aeim { aeim_estate = Just estate }
        addLine aeim (Statement (GenericLhs "desc") (GenericRhs desc)) = aeim { aeim_desc = Just desc }
        addLine aeim (Statement (GenericLhs "influence") (FloatRhs influence)) = aeim { aeim_influence = Just influence }
        addLine aeim (Statement (GenericLhs "duration") (FloatRhs duration)) = aeim { aeim_duration = Just duration }
        addLine aeim _ = aeim
        pp_eim :: AddEstateInfluenceModifier -> PP extra ScriptMessage
        pp_eim aeim
            = case (aeim_estate aeim, aeim_desc aeim, aeim_influence aeim, aeim_duration aeim) of
                (Just estate, Just desc, Just influence, Just duration) -> do
                    let estate_icon = iconText estate
                    estate_loc <- getGameL10n estate
                    desc_loc <- getGameL10n desc
                    dur <- timeOrIndef duration
                    return (msg estate_icon estate_loc desc_loc influence dur)
                _ -> return (preMessage stmt)
estateInfluenceModifier _ stmt = preStatement stmt

data EstateInfluence = EstateInfluence {
        ei_estate :: Maybe Text
    ,   ei_influence :: Maybe Double
    } deriving Show
newEstateInfluence = EstateInfluence Nothing Nothing

estateInfluence :: (Text -> Text -> Double -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
estateInfluence msg stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_eim $ foldl' addLine newEstateInfluence scr)
    where
        addLine :: EstateInfluence -> GenericStatement -> EstateInfluence 
        addLine ei (Statement (GenericLhs "estate") (GenericRhs estate)) = ei { ei_estate = Just estate }
        addLine ei (Statement (GenericLhs "influence") (FloatRhs influence)) = ei { ei_influence = Just influence }
        addLine ei _ = ei
        pp_eim :: EstateInfluence -> PP extra ScriptMessage
        pp_eim ei
            = case (ei_estate ei, ei_influence ei) of
                (Just estate, Just influence) -> do
                    let estate_icon = iconText estate
                    estate_loc <- getGameL10n estate
                    return (msg estate_icon estate_loc influence)
                _ -> return (preMessage stmt)
estateInfluence _ stmt = preStatement stmt

data EstateLoyalty = EstateLoyalty {
        el_estate :: Maybe Text
    ,   el_loyalty :: Maybe Double
    } deriving Show
newEstateLoyalty = EstateLoyalty Nothing Nothing

estateLoyalty :: (Text -> Text -> Double -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
estateLoyalty msg stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_elm $ foldl' addLine newEstateLoyalty scr)
    where
        addLine :: EstateLoyalty -> GenericStatement -> EstateLoyalty 
        addLine el (Statement (GenericLhs "estate") (GenericRhs estate)) = el { el_estate = Just estate }
        addLine el (Statement (GenericLhs "loyalty") (FloatRhs loyalty)) = el { el_loyalty = Just loyalty }
        addLine el _ = el
        pp_elm :: EstateLoyalty -> PP extra ScriptMessage
        pp_elm el
            = case (el_estate el, el_loyalty el) of
                (Just estate, Just loyalty) -> do
                    let estate_icon = iconText estate
                    estate_loc <- getGameL10n estate
                    return (msg estate_icon estate_loc loyalty)
                _ -> return (preMessage stmt)
estateLoyalty _ stmt = preStatement  stmt

data Heir = Heir
        {   heir_dynasty :: Maybe Text
        ,   heir_claim :: Maybe Double
        ,   heir_age :: Maybe Double
        }
newHeir = Heir Nothing Nothing Nothing
defineHeir :: GenericStatement -> PP extra [(Int, ScriptMessage)]
defineHeir stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_heir $ foldl' addLine newHeir scr)
    where
        addLine :: Heir -> GenericStatement -> Heir 
        addLine heir (Statement (GenericLhs "dynasty") (GenericRhs dynasty)) = heir { heir_dynasty = Just dynasty }
        addLine heir (Statement (GenericLhs "claim") (FloatRhs claim)) = heir { heir_claim = Just claim }
        addLine heir (Statement (GenericLhs "age") (FloatRhs age)) = heir { heir_age = Just age }
        addLine heir _ = heir
        pp_heir :: Heir -> PP extra ScriptMessage
        pp_heir heir = do
            dynasty_flag <- fmap doc2text <$> maybeM flag (heir_dynasty heir)
            case (heir_age heir, dynasty_flag, heir_claim heir) of
                (Nothing,  Nothing,   Nothing)    -> return $ MsgNewHeir
                (Nothing,  Nothing,   Just claim) -> return $ MsgNewHeirClaim claim
                (Nothing,  Just flag, Nothing)    -> return $ MsgNewHeirDynasty flag
                (Nothing,  Just flag, Just claim) -> return $ MsgNewHeirDynastyClaim flag claim
                (Just age, Nothing,   Nothing)    -> return $ MsgNewHeirAge age
                (Just age, Nothing,   Just claim) -> return $ MsgNewHeirAgeClaim age claim
                (Just age, Just flag, Nothing)    -> return $ MsgNewHeirAgeFlag age flag
                (Just age, Just flag, Just claim) -> return $ MsgNewHeirAgeFlagClaim age flag claim
defineHeir stmt = preStatement stmt

data NumOfReligion = NumOfReligion 
        {   nor_religion :: Maybe Text
        ,   nor_value :: Maybe Double
        }
newNOR = NumOfReligion Nothing Nothing
numOfReligion :: GenericStatement -> PP extra [(Int, ScriptMessage)]
numOfReligion stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_heir $ foldl' addLine newNOR scr)
    where
        addLine :: NumOfReligion -> GenericStatement -> NumOfReligion
        addLine nor (Statement (GenericLhs "religion") rhs) | Just religion <- textRhs rhs = nor { nor_religion = Just religion }
        addLine nor (Statement (GenericLhs "value") rhs) | Just val <- floatRhs rhs = nor { nor_value = Just val }
        addLine heir _ = heir
        pp_heir :: NumOfReligion -> PP extra ScriptMessage
        pp_heir nor = case (nor_religion nor, nor_value nor) of
            (Just religion, Just value) -> do
                rel_loc <- getGameL10n religion
                let rel_icon = iconText religion
                return (MsgNumOfReligion rel_icon rel_loc value)
            _ -> return (preMessage stmt)

-- Assume 1 <= n <= 8
hreReformLoc :: Int -> PP extra Text
hreReformLoc n = getGameL10n $ case n of
    1 -> "reichsreform_title"
    2 -> "reichsregiment_title"
    3 -> "hofgericht_title"
    4 -> "gemeinerpfennig_title"
    5 -> "landfriede_title"
    6 -> "erbkaisertum_title"
    7 -> "privilegia_de_non_appelando_title"
    8 -> "renovatio_title"

hreReformLevel :: GenericStatement -> PP extra [(Int, ScriptMessage)]
hreReformLevel (Statement _ rhs) | Just level <- floatRhs rhs, level >= 0, level <= 8
    = if level == 0
        then msgToPP MsgNoHREReforms
        else msgToPP . MsgHREPassedReform =<< hreReformLoc level
hreReformLevel stmt = preStatement stmt

religionYears :: GenericStatement -> PP extra [(Int, ScriptMessage)]
religionYears (Statement _ (CompoundRhs [Statement (GenericLhs rel) rhs]))
    | Just years <- floatRhs rhs = do
        let rel_icon = iconText rel
        rel_loc <- getGameL10n rel
        msgToPP $ MsgReligionYears rel_icon rel_loc years
religionYears stmt = preStatement stmt

----------------------
-- Idea group ideas --
----------------------

hasIdea :: (Text -> Int -> ScriptMessage) -> GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
hasIdea msg stmt@(Statement (GenericLhs lhs) rhs) | Just n <- floatRhs rhs, n >= 1, n <= 7 = do
    mgroupTable <- asks info
    case mgroupTable of
        Nothing -> do -- can't lookup ideas!
            err <- plainMsg "(warning: couldn't look up idea groups)"
            premsg <- preStatement stmt
            return (err ++ premsg)
        Just groupTable ->
            let mideagroup = HM.lookup lhs groupTable
            in case mideagroup of
                Nothing -> preStatement stmt -- unknown idea group
                Just group -> do
                    let idea = ig_ideas group !! (n - 1)
                        ideaKey = idea_name idea
                    idea_loc <- getGameL10n ideaKey
                    msgToPP (msg idea_loc n)
