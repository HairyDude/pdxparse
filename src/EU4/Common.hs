{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables #-}
module EU4.Common (
        pp_script
    ,   pp_mtth
    ,   ppOne
    ,   ppMany
    ,   IdeaTable
    ,   iconKey, iconFile, iconFileB
    ,   AIWillDo (..), AIModifier (..)
    ,   ppAiWillDo, ppAiMod
    ,   module EU4.SuperCommon
    ) where

import Prelude hiding (sequence, mapM)

import Debug.Trace

import Control.Arrow
import Control.Monad.Reader hiding (sequence, mapM, forM)

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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
import MessageTools (plural)
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
pp_script script = imsg2doc =<< ppMany script

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
pp_statement stmt = imsg2doc =<< ppOne stmt

ppMany :: GenericScript -> PP IdeaTable [(Int, ScriptMessage)]
ppMany scr = indentUp (concat <$> mapM ppOne scr)

ppOne :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
ppOne stmt@(Statement lhs rhs) = case lhs of
    GenericLhs label -> case T.map toLower label of
        -- Statements where RHS is irrelevant (usually "yes")
        "add_cardinal"          -> msgToPP MsgAddCardinal
        "cb_on_primitives"      -> msgToPP MsgGainPrimitivesCB
        "cb_on_religious_enemies" -> msgToPP MsgGainReligiousCB
        "enable_hre_leagues"    -> msgToPP MsgEnableHRELeagues
        "kill_heir"             -> msgToPP MsgHeirDies
        "kill_ruler"            -> msgToPP MsgRulerDies
        "may_explore"           -> msgToPP MsgMayExplore
        "set_hre_religion_treaty" -> msgToPP MsgSignWestphalia
        "remove_cardinal"       -> msgToPP MsgLoseCardinal
        "sea_repair"            -> msgToPP MsgGainSeaRepair
        -- Gain/lose, with optional icon
        --  numbers
        "add_adm_power"         -> gainIcon "adm" MsgGainADM stmt
        "add_army_tradition"    -> gainIcon "army tradition" MsgGainAT stmt
        "add_authority"         -> gain' MsgGainAuth stmt
        "add_base_tax"          -> gainIcon "base tax" MsgGainBT stmt
        "add_base_production"   -> gainIcon "production" MsgGainBP stmt
        "add_base_manpower"     -> gainIcon "manpower" MsgGainBM stmt
        "add_dip_power"         -> gainIcon "dip" MsgGainDIP stmt
        "add_doom"              -> gain' MsgGainDoom stmt
        "add_heir_claim"        -> gain' MsgHeirGainClaim stmt
        "add_devotion"          -> gainIcon "devotion" MsgGainDevotion stmt
        "add_horde_unity"        -> gainIcon "horde unity" MsgGainHordeUnity stmt
        "add_imperial_influence" -> gainIcon "imperial authority" MsgGainImperialAuthority stmt
        "add_karma"              -> gainIcon "high karma" MsgGainKarma stmt
        "add_legitimacy"         -> gainIcon "legitimacy" MsgGainLegitimacy stmt
        "add_mil_power"          -> gainIcon "mil" MsgGainMIL stmt
        "add_navy_tradition"     -> gainIcon "navy tradition" MsgGainNavyTradition stmt
        "add_papal_influence"    -> gainIcon "papal influence" MsgGainPapalInfluence stmt
        "add_prestige"           -> gainIcon "prestige" MsgGainPrestige stmt
        "add_stability"          -> gainIcon "stability" MsgGainStability stmt
        "add_war_exhaustion"     -> gainIcon "war exhaustion" MsgGainWarExhaustion stmt
        "add_yearly_manpower"    -> gainIcon "manpower" MsgGainYearlyManpower stmt
        "change_adm"             -> gainIcon "adm" MsgGainADMSkill stmt
        "change_dip"             -> gainIcon "dip" MsgGainDIPSkill stmt
        "change_mil"             -> gainIcon "mil" MsgGainMILSkill stmt
        "change_siege"           -> gain' MsgGainSiegeProgress stmt
        -- Used in ideas and other bonuses, omit "gain/lose" in l10n
        "colonists"              -> gainIcon "colonist" MsgGainColonists stmt
        "blockade_efficiency"    -> gainIcon "blockade efficiency" MsgGainBlockadeEfficiency stmt
        "build_cost"             -> gainIcon "build cost" MsgGainBuildCost stmt
        "church_power_modifier"  -> gainIcon "church power" MsgGainChurchPowerModifier stmt
        "culture_conversion_cost" -> gainIcon "culture conversion cost" MsgGainCultureConvCost stmt
        "development_cost"       -> gainIcon "development cost" MsgGainDevelCost stmt
        "devotion"               -> gainIcon "devotion" MsgGainYearlyDevotion stmt
        "global_autonomy"        -> gainIcon "global autonomy" MsgGainGlobalAutonomy stmt
        "global_colonial_growth" -> gainIcon "global settler increase" MsgGainGlobalSettlers stmt
        "global_missionary_strength" -> gainIcon "missionary strength" MsgGainMissionaryStrength stmt
        "global_ship_repair"     -> gainIcon "global ship repair" MsgGainGlobalShipRepair stmt
        "global_ship_cost"       -> gainIcon "ship cost" MsgGainGlobalShipCost stmt
        "global_tax_modifier"    -> gainIcon "global tax modifier" MsgGainGlobalTaxModifier stmt
        "global_tariffs"         -> gainIcon "global tariffs" MsgGainGlobalTariffs stmt
        "inflation_reduction"    -> gainIcon "inflation reduction" MsgGainYearlyInflationReduction stmt
        "interest"               -> gainIcon "interest" MsgGainInterestPerAnnum stmt
        "land_maintenance_modifier" -> gainIcon "land maintenance" MsgGainLandMaintenanceMod stmt
        "leader_naval_manuever" {- sic -} -> gainIcon "naval forcelimit" MsgGainNavalLeaderManeuver stmt
        "light_ship_power"       -> gainIcon "light ship combat ability" MsgGainLightShipPower stmt
        "missionaries"           -> gainIcon "colonist" MsgGainMissionaries stmt
        "global_heretic_missionary_strength" -> gainIcon "missionary strength vs heretics" MsgGainMissionaryStrengthVsHeretics stmt
        "monthly_fervor_increase" -> gainIcon "monthly fervor" MsgGainMonthlyFervor stmt
        "naval_forcelimit_modifier" -> gainIcon "naval forcelimit" MsgGainNavalForcelimitMod stmt
        "navy_tradition"         -> gainIcon "navy tradition" MsgGainYearlyNavyTradition stmt
        "papal_influence"        -> gainIcon "papal influence" MsgGainYearlyPapalInfluence stmt
        "prestige"               -> gainIcon "prestige" MsgGainYearlyPrestige stmt
        "production_efficiency"  -> gainIcon "production efficiency" MsgGainProdEff stmt
        "range"                  -> gainIcon "colonial range" MsgGainGlobalTariffs stmt
        "stability_cost_modifier" -> gainIcon "stability cost" MsgGainStabilityCost stmt
        "tolerance_own"          -> gainIcon "tolerance of the true faith" MsgGainToleranceTrue stmt
        -- numbers
        "add_patriarch_authority"  -> gainIcon "patriarch authority" MsgGainPatAuth stmt
        "add_piety"                -> gainIcon "piety" MsgGainPiety stmt
        "add_republican_tradition" -> gainIcon "republican tradition" MsgGainRepTrad stmt
        -- Percentages
        "add_inflation"      -> gainIcon "inflation" MsgGainInflation stmt
        "add_local_autonomy" -> gainIcon "local autonomy" MsgGainLocalAutonomy stmt
        "add_reform_desire"  -> gainIcon "reform desire" MsgGainReformDesire stmt
        "add_mercantilism"   -> gainIcon "mercantilism" MsgGainMercantilism stmt
        -- Special
        "add_manpower" -> gainManpower stmt
        "is_month" -> isMonth stmt
        -- Modifiers
        "add_country_modifier"      -> addModifier MsgCountryMod stmt
        "add_permanent_province_modifier" -> addModifier MsgPermanentProvMod stmt
        "add_province_modifier"     -> addModifier MsgProvMod stmt
        "add_ruler_modifier"        -> addModifier MsgRulerMod stmt
        "add_trade_modifier"        -> addModifier MsgTradeMod stmt
        "has_country_modifier"      -> withLocAtom2 MsgCountryMod MsgHasModifier stmt
        "has_province_modifier"     -> withLocAtom2 MsgProvMod MsgHasModifier stmt
        "has_ruler_modifier"        -> withLocAtom2 MsgRulerMod MsgHasModifier stmt
        "has_trade_modifier"        -> tradeMod stmt
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
        "any_enemy_country"         -> compoundMessage MsgAnyEnemyCountry stmt
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
        "every_rival_country"       -> compoundMessage MsgEveryRival stmt
        "every_subject_country"     -> compoundMessage MsgEverySubject stmt
        "hidden_effect"             -> compoundMessage MsgHiddenEffect stmt
        "if"                        -> compoundMessage MsgIf stmt
        "limit"                     -> compoundMessage MsgLimit stmt
        "owner"                     -> compoundMessage MsgOwner stmt
        "random_active_trade_node"  -> compoundMessage MsgRandomActiveTradeNode stmt
        "random_ally"               -> compoundMessage MsgRandomAlly stmt
        "random_core_country"       -> compoundMessage MsgRandomCoreCountry stmt
        "random_country"            -> compoundMessage MsgRandomCountry stmt
        "random_known_country"      -> compoundMessage MsgRandomKnownCountry stmt
        "random_list"               -> compoundMessage MsgRandom stmt
        "random_neighbor_country"   -> compoundMessage MsgRandomNeighborCountry stmt
        "random_neighbor_province"  -> compoundMessage MsgRandomNeighborProvince stmt
        "random_owned_province"     -> compoundMessage MsgRandomOwnedProvince stmt
        "random_province"           -> compoundMessage MsgRandomProvince stmt
        "random_rival_country"      -> compoundMessage MsgRandomRival stmt
        -- Random
        "random" -> random stmt
        -- Simple generic statements (RHS is a localizable atom)
        "change_government" -> withLocAtom MsgChangeGovernment stmt
        "continent"         -> withLocAtom MsgContinentIs stmt
        "change_culture"    -> withLocAtom MsgChangeCulture stmt
        "culture"           -> withLocAtom MsgCultureIs stmt
        "culture_group"     -> withLocAtom MsgCultureIsGroup stmt
        "dynasty"           -> withLocAtom MsgRulerIsDynasty stmt
        "end_disaster"      -> withLocAtom MsgDisasterEnds stmt
        "government"        -> withLocAtom MsgGovernmentIs stmt
        "has_advisor"       -> withLocAtom MsgHasAdvisor stmt
        "has_active_policy" -> withLocAtom MsgHasActivePolicy stmt
        "has_disaster"      -> withLocAtom MsgDisasterOngoing stmt
        "has_idea"          -> withLocAtom MsgHasIdea stmt
        "has_terrain"       -> withLocAtom MsgHasTerrain stmt 
        "infantry"          -> withLocAtom MsgInfantrySpawns stmt
        "kill_advisor"      -> withLocAtom MsgAdvisorDies stmt
        "primary_culture"   -> withLocAtom MsgPrimaryCultureIs stmt
        "region"            -> withLocAtom MsgRegionIs stmt
        "remove_advisor"    -> withLocAtom MsgLoseAdvisor stmt
        -- RHS is a province ID
        "capital"            -> withProvince MsgCapitalIs stmt
        "owns"               -> withProvince MsgOwns stmt
        "owns_core_province" -> withProvince MsgOwnsCore stmt
        "province_id"        -> withProvince MsgProvinceIs stmt
        -- RHS is a flag OR a province ID
        "remove_core"      -> withFlagOrProvince MsgLoseCoreCountry MsgLoseCoreProvince stmt
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
        "change_technology_group" -> withLocAtomIcon MsgChangeTechGroup stmt
        "change_trade_goods" -> withLocAtomIcon MsgChangeGoods stmt
        "change_unit_type"   -> withLocAtomIcon MsgChangeUnitType stmt
        "create_advisor"     -> withLocAtomIcon MsgCreateAdvisor stmt
        "dominant_religion"  -> withLocAtomIcon MsgDominantReligion stmt
        "has_building"       -> withLocAtomIcon MsgHasBuilding stmt
        "has_idea_group"     -> withLocAtomIcon MsgHasIdeaGroup stmt -- FIXME: icon fails
        "full_idea_group"    -> withLocAtomIcon MsgFullIdeaGroup stmt
        "hre_religion"       -> withLocAtomIcon MsgHREReligion stmt
        "is_religion_enabled" -> withLocAtomIcon MsgReligionEnabled stmt
        "remove_estate"      -> withLocAtomIcon MsgRemoveFromEstate stmt 
        "secondary_religion" -> withLocAtomIcon MsgSecondaryReligion stmt
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
        "change_tag"         -> withFlag MsgChangeTag stmt
        "controlled_by"      -> withFlag MsgControlledBy stmt
        "defensive_war_with" -> withFlag MsgDefensiveWarAgainst stmt
        "discover_country"   -> withFlag MsgDiscoverCountry stmt
        "add_claim"          -> withFlag MsgGainClaim stmt
        "add_permanent_claim" -> withFlag MsgGainPermanentClaim stmt
        "create_alliance"    -> withFlag MsgCreateAlliance stmt
        "galley"             -> withFlag MsgGalley stmt
        "has_discovered"     -> withFlag MsgHasDiscovered stmt
        "heavy_ship"         -> withFlag MsgHeavyShip stmt
        "inherit"            -> withFlag MsgInherit stmt
        "is_neighbor_of"     -> withFlag MsgNeighbors stmt
        "is_league_enemy"    -> withFlag MsgIsLeagueEnemy stmt
        "is_subject_of"      -> withFlag MsgIsSubjectOf stmt
        "light_ship"         -> withFlag MsgLightShip stmt
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
        "exists"            -> withFlagOrBool MsgExists MsgCountryExists stmt
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
        "has_any_disaster"       -> withBool MsgHasAnyDisaster stmt
        "has_cardinal"           -> withBool MsgHasCardinal stmt
        "has_heir"               -> withBool MsgHasHeir stmt
        "has_owner_culture"      -> withBool MsgHasOwnerCulture stmt
        "has_owner_religion"     -> withBool MsgHasOwnerReligion stmt
        "has_parliament"         -> withBool MsgHasParliament stmt
        "has_port"               -> withBool MsgHasPort stmt
        "has_seat_in_parliament" -> withBool MsgHasSeatInParliament stmt
        "has_regency"            -> withBool MsgIsInRegency stmt
        "has_siege"              -> withBool MsgUnderSiege stmt
        "has_secondary_religion" -> withBool MsgHasSecondaryReligion stmt
        "has_truce"              -> withBool MsgHasTruce stmt
        "has_wartaxes"           -> withBool MsgHasWarTaxes stmt
        "hre_leagues_enabled"    -> withBool MsgHRELeaguesEnabled stmt
        "hre_religion_locked"    -> withBool MsgHREReligionLocked stmt
        "hre_religion_treaty"    -> withBool MsgHREWestphalia stmt
        "is_at_war"              -> withBool MsgAtWar stmt
        "is_bankrupt"            -> withBool MsgIsBankrupt stmt
        "is_capital"             -> withBool MsgIsCapital stmt
        "is_city"                -> withBool MsgIsCity stmt
        "is_colony"              -> withBool MsgIsColony stmt
        "is_colonial_nation"     -> withBool MsgIsColonialNation stmt
        "is_defender_of_faith"   -> withBool MsgIsDefenderOfFaith stmt
        "is_former_colonial_nation" -> withBool MsgIsFormerColonialNation stmt
        "is_elector"             -> withBool MsgIsElector stmt
        "is_emperor"             -> withBool MsgIsEmperor stmt
        "is_female"              -> withBool MsgIsFemale stmt
        "is_in_league_war"       -> withBool MsgIsInLeagueWar stmt
        "is_lesser_in_union"     -> withBool MsgIsLesserInUnion stmt
        "is_looted"              -> withBool MsgIsLooted stmt
        "is_nomad"               -> withBool MsgIsNomad stmt
        "is_overseas"            -> withBool MsgIsOverseas stmt
        "is_part_of_hre"         -> withBool MsgIsPartOfHRE stmt
        "is_playing_custom_nation" -> withBool MsgIsCustomNation stmt
        "is_random_new_world"    -> withBool MsgRandomNewWorld stmt
        "is_reformation_center"  -> withBool MsgIsCenterOfReformation stmt
        "is_religion_reformed"   -> withBool MsgReligionReformed stmt
        "is_sea"                 -> withBool MsgIsSea stmt -- province or trade node
        "is_subject"             -> withBool MsgIsSubject stmt
        "luck"                   -> withBool MsgLucky stmt
        "normal_or_historical_nations" -> withBool MsgNormalOrHistoricalNations stmt
        "papacy_active"          -> withBool MsgPapacyIsActive stmt
        "primitives"             -> withBool MsgPrimitives stmt
        "set_hre_religion_locked" -> withBool MsgSetHREReligionLocked stmt
        "set_in_empire"          -> withBool MsgSetInEmpire stmt
        "unit_in_siege"          -> withBool MsgUnderSiege stmt -- duplicate?
        "was_player"             -> withBool MsgHasBeenPlayer stmt
        -- Numeric statements
        "colonysize"                -> numeric MsgColonySettlers stmt
        "had_recent_war"            -> numeric MsgWasAtWar stmt
        "heir_age"                  -> numeric MsgHeirAge stmt
        "is_year"                   -> numeric MsgYearIs stmt
        "num_of_colonial_subjects"  -> numeric MsgNumColonialSubjects stmt
        "num_of_colonies"           -> numeric MsgNumColonies stmt
        "num_of_loans"              -> numeric MsgNumLoans stmt
        "num_of_mercenaries"        -> numeric MsgNumMercs stmt
        "num_of_ports"              -> numeric MsgNumPorts stmt
        "num_of_rebel_armies"       -> numeric MsgNumRebelArmies stmt
        "num_of_rebel_controlled_provinces" -> numeric MsgNumRebelControlledProvinces stmt
        "num_of_trade_embargos"     -> numeric MsgNumEmbargoes stmt
        "revolt_percentage"         -> numeric MsgRevoltPercentage stmt
        "trade_income_percentage"   -> numeric MsgTradeIncomePercentage stmt
        "units_in_province"         -> numeric MsgUnitsInProvince stmt
        -- Statements that may be numeric or a tag
        "num_of_cities"             -> numericOrTag MsgNumCities MsgNumCitiesThan stmt
        -- Signed numeric statements
        "tolerance_to_this" -> numeric MsgToleranceToThis stmt
        -- Special cases
        "legitimacy_or_horde_unity" -> numeric MsgLegitimacyOrHordeUnity stmt
        -- Statements of numeric quantities with icons
        "add_treasury"          -> numericIcon "ducats" MsgAddTreasury stmt
        "add_unrest"            -> numericIcon "local unrest" MsgAddLocalUnrest stmt
        "add_years_of_income"   -> numericIcon "ducats" MsgAddYearsOfIncome stmt
        "adm"                   -> numericIcon "adm" MsgRulerADM stmt
        "adm_power"             -> numericIcon "adm" MsgHasADM stmt
        "adm_tech"              -> numericIcon "adm tech" MsgADMTech stmt
        "army_tradition"        -> numericIcon "army tradition" MsgArmyTradition stmt
        "base_manpower"         -> numericIcon "navy tradition" MsgBaseManpower stmt
        "base_production"       -> numericIcon "base production" MsgBaseProduction stmt
        "base_tax"              -> numericIcon "base tax" MsgBaseTax stmt
        "blockade"              -> numericIcon "blockade" MsgBlockade stmt
        "create_admiral"        -> numericIcon "admiral" MsgCreateAdmiral stmt
        "create_conquistador"   -> numericIcon "conquistador" MsgCreateConquistador stmt
        "create_explorer"       -> numericIcon "explorer" MsgCreateExplorer stmt
        "create_general"        -> numericIcon "general" MsgCreateGeneral stmt
        "development"           -> numericIcon "development" MsgDevelopment stmt
        "dip"                   -> numericIcon "dip" MsgRulerDIP stmt
        "dip_power"             -> numericIcon "adm" MsgHasDIP stmt
        "dip_tech"              -> numericIcon "dip tech" MsgDIPTech stmt
        "fort_level"            -> numericIcon "fort level" MsgFortLevel stmt
        "gold_income_percentage" -> numericIcon "gold" MsgGoldIncomePercentage stmt
        "horde_unity"           -> numericIcon "horde unity" MsgHordeUnity stmt
        "inflation"             -> numericIcon "inflation" MsgInflation stmt
        "karma"                 -> numericIcon "high karma" MsgKarma stmt
        "legitimacy"            -> numericIcon "legitimacy" MsgLegitimacy stmt
        "local_autonomy"        -> numericIcon "local autonomy" MsgLocalAutonomy stmt
        "manpower_percentage"   -> numericIcon "manpower" MsgManpowerPercentage stmt
        "mercantilism"          -> numericIcon "mercantilism" MsgMercantilism stmt
        "mil"                   -> numericIcon "mil" MsgRulerMIL stmt
        "mil_power"             -> numericIcon "adm" MsgHasMIL stmt
        "mil_tech"              -> numericIcon "mil tech" MsgMILTech stmt
        "monthly_income"        -> numericIcon "ducats" MsgMonthlyIncome stmt
        "naval_forcelimit"      -> numericIcon "naval force limit" MsgNavalForcelimit stmt
        "num_of_allies"         -> numericIcon "alliance" MsgNumAllies stmt
        "num_of_cardinals"      -> numericIcon "cardinal" MsgNumCardinals stmt
        "num_of_colonists"      -> numericIcon "colonists" MsgNumColonists stmt
        "num_of_heavy_ship"     -> numericIcon "heavy ship" MsgNumHeavyShips stmt
        "num_of_merchants"      -> numericIcon "merchant" MsgNumMerchants stmt
        "num_of_royal_marriages" -> numericIcon "royal marriage" MsgNumRoyalMarriages stmt
        "overextension_percentage" -> numericIcon "overextension" MsgOverextension stmt
        "reform_desire"         -> numericIcon "reform desire" MsgReformDesire stmt
        "religious_unity"       -> numericIcon "religious unity" MsgReligiousUnity stmt
        "republican_tradition"  -> numericIcon "republican tradition" MsgRepTrad stmt
        "stability"             -> numericIcon "stability" MsgStability stmt
        "total_development"     -> numericIcon "development" MsgTotalDevelopment stmt
        "total_number_of_cardinals" -> numericIcon "cardinal" MsgTotalCardinals stmt
        "trade_efficiency"      -> numericIcon "trade efficiency" MsgTradeEfficiency stmt
        "treasury"              -> numericIcon "ducats" MsgHasDucats stmt
        "unrest"               -> numericIcon "unrest" MsgUnrest stmt
        "war_exhaustion"       -> numericIcon "war exhaustion" MsgWarExhaustion stmt
        "war_score"            -> numericIcon "war score" MsgWarScore stmt
        -- As above - advisors
        "army_reformer" -> numericIcon "army reformer" MsgHasArmyReformerLevel stmt
        "artist" -> numericIcon "artist" MsgHasArtistLevel stmt
        "diplomat" -> numericIcon "diplomat" MsgHasDiplomatLevel stmt
        "natural_scientist" -> numericIcon "natural scientist" MsgHasNaturalScientistLevel stmt
        "naval_reformer" -> numericIcon "naval reformer" MsgHasNavyReformerLevel stmt
        "navy_reformer" -> numericIcon "naval reformer" MsgHasNavyReformerLevel stmt
        "statesman" -> numericIcon "statesman" MsgHasStatesmanLevel stmt
        "theologian" -> numericIcon "theologian" MsgHasTheologianLevel stmt
        "trader" -> numericIcon "trader" MsgHasTraderLevel stmt
        -- Number of provinces of some kind, mostly religions and trade goods
        "orthodox" -> numProvinces "orthodox" MsgReligionProvinces stmt
        "cloth" -> numProvinces "cloth" MsgGoodsProvinces stmt
        "chinaware" -> numProvinces "chinaware" MsgGoodsProvinces stmt
        "copper" -> numProvinces "copper" MsgGoodsProvinces stmt
        "fish" -> numProvinces "fish" MsgGoodsProvinces stmt
        "fur" -> numProvinces "fur" MsgGoodsProvinces stmt
        "gold" -> numProvinces "gold" MsgGoodsProvinces stmt
        "grain" -> numProvinces "grain" MsgGoodsProvinces stmt
        "iron" -> numProvinces "iron" MsgGoodsProvinces stmt
        "ivory" -> numProvinces "ivory" MsgGoodsProvinces stmt
        "naval_supplies" -> numProvinces "fish" MsgGoodsProvinces stmt
        "salt" -> numProvinces "salt" MsgGoodsProvinces stmt
        "slaves" -> numProvinces "slaves" MsgGoodsProvinces stmt
        "spices" -> numProvinces "spices" MsgGoodsProvinces stmt
        "wine" -> numProvinces "wine" MsgGoodsProvinces stmt
        "wool" -> numProvinces "wool" MsgGoodsProvinces stmt
        -- Complex statements
        "add_casus_belli"                   -> addCB True stmt
        "add_faction_influence"             -> factionInfluence stmt
        "add_estate_loyalty"                -> textValue "estate" "loyalty" MsgAddEstateLoyalty MsgAddEstateLoyalty tryLoc stmt
        "add_estate_influence_modifier"     -> estateInfluenceModifier MsgEstateInfluenceModifier stmt
        "add_opinion"                       -> opinion MsgAddOpinion MsgAddOpinionDur stmt
        "reverse_add_opinion"               -> opinion MsgReverseAddOpinion MsgReverseAddOpinionDur stmt
        "area"                              -> case rhs of
            CompoundRhs _ -> compoundMessage MsgArea stmt
            _             -> withLocAtom MsgAreaIs stmt
        "define_heir"                       -> defineHeir stmt
        "build_to_forcelimit"               -> buildToForcelimit stmt
        "check_variable"                    -> textValue "which" "value" MsgCheckVariable MsgCheckVariable tryLoc stmt
        "country_event"                     -> triggerEvent MsgCountryEvent stmt
        "declare_war_with_cb"               -> declareWarWithCB stmt
        "define_advisor"                    -> defineAdvisor stmt
        "define_ruler"                      -> defineRuler stmt
        "estate_influence"                  -> textValue "estate" "influence" MsgEstateInfluence MsgEstateInfluence tryLoc stmt
        "estate_loyalty"                    -> textValue "estate" "loyalty" MsgEstateLoyalty MsgEstateLoyalty tryLoc stmt
        "had_country_flag"                  -> textValue "flag" "days" MsgHadCountryFlag MsgHadCountryFlag tryLoc stmt
        "had_global_flag"                   -> textValue "flag" "days" MsgHadGlobalFlag MsgHadGlobalFlag tryLoc stmt
        "had_province_flag"                 -> textValue "flag" "days" MsgHadProvinceFlag MsgHadProvinceFlag tryLoc stmt
        "had_ruler_flag"                    -> textValue "flag" "days" MsgHadRulerFlag MsgHadRulerFlag tryLoc stmt
        "has_estate_influence_modifier"     -> hasEstateInfluenceModifier stmt
        "has_opinion"                       -> hasOpinion stmt
        "has_opinion_modifier"              -> opinion MsgHasOpinionMod (\what who _years -> MsgHasOpinionMod what who) stmt
        "province_event"                    -> triggerEvent MsgProvinceEvent stmt
        "remove_opinion"                    -> opinion MsgRemoveOpinionMod (\what who _years -> MsgRemoveOpinionMod what who) stmt
        "religion_years"                    -> religionYears stmt
        "reverse_add_casus_belli"           -> addCB False stmt
        "trigger_switch"                    -> triggerSwitch stmt
        "num_of_religion"                   -> textValue "religion" "value" MsgNumOfReligion MsgNumOfReligion tryLoc stmt
        -- Rebels
        "can_spawn_rebels"   -> canSpawnRebels stmt
        "create_revolt"      -> spawnRebels Nothing stmt
        "has_spawned_rebels" -> hasSpawnedRebels stmt
        "likely_rebels"      -> canSpawnRebels stmt
        "spawn_rebels"       -> spawnRebels Nothing stmt
        -- Specific rebels
        "anti_tax_rebels"    -> spawnRebels (Just "anti_tax_rebels") stmt
        "nationalist_rebels" -> spawnRebels (Just "nationalist_rebels") stmt
        "noble_rebels"       -> spawnRebels (Just "noble_rebels") stmt
        -- Idea groups
        "aristocracy_ideas"    -> hasIdea MsgHasAristocraticIdea stmt
        "defensive_ideas"      -> hasIdea MsgHasDefensiveIdea stmt
        "economic_ideas"       -> hasIdea MsgHasEconomicIdea stmt
        "innovativeness_ideas" -> hasIdea MsgHasInnovativeIdea stmt
        "maritime_ideas"      -> hasIdea MsgHasMaritimeIdea stmt
        "offensive_ideas"      -> hasIdea MsgHasOffensiveIdea stmt
        -- Special
        "add_core"  -> addCore stmt
        "government_rank" -> govtRank stmt
        "set_government_rank" -> setGovtRank stmt
        "has_dlc"   -> hasDlc stmt
        "hre_reform_level" -> hreReformLevel stmt
        -- Ignored
        "custom_tooltip" -> plainMsg "(custom tooltip - delete this line)"
        "tooltip" -> plainMsg "(explanatory tooltip - delete this line)"
        -- default
        _ -> if isTag label
             then case rhs of
                CompoundRhs scr -> 
                    withCurrentIndent $ \_ -> do -- force indent level at least 1
                        [lflag] <- plainMsg =<< (<> ":") <$> flagText label
                        scriptMsgs <- ppMany scr
                        return (lflag : scriptMsgs)
                _ -> preStatement stmt
             else do
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
                scriptMsgs <- ppMany scr
                return (header ++ scriptMsgs)
            _ -> preStatement stmt
    CustomLhs _ -> preStatement stmt
ppOne stmt = preStatement stmt


------------------------------------------------------------------------
-- Script handlers that should be used directly, not via pp_statement --
------------------------------------------------------------------------

data MTTH = MTTH
        {   mtth_years :: Maybe Int
        ,   mtth_months :: Maybe Int
        ,   mtth_days :: Maybe Int
        ,   mtth_modifiers :: [MTTHModifier] -- TODO
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
        addField mtth (Statement (GenericLhs "years") rhs) | Just n <- floatRhs rhs
            = mtth { mtth_years = Just n }
        addField mtth (Statement (GenericLhs "months") rhs) | Just n <- floatRhs rhs
            = mtth { mtth_months = Just n }
        addField mtth (Statement (GenericLhs "days") rhs) | Just n <- floatRhs rhs
            = mtth { mtth_days = Just n }
        addField mtth (Statement (GenericLhs "modifier") (CompoundRhs rhs))
            = addMTTHMod mtth rhs
        addField mtth _ = mtth -- unrecognized
        addMTTHMod mtth scr = mtth { mtth_modifiers = mtth_modifiers mtth ++ [foldl' addMTTHModField newMTTHMod scr] } where
            addMTTHModField mtthmod (Statement (GenericLhs "factor") rhs)
                = mtthmod { mtthmod_factor = floatRhs rhs }
            addMTTHModField mtthmod stmt -- anything else is a condition
                = mtthmod { mtthmod_conditions = mtthmod_conditions mtthmod ++ [stmt] }
        pp_mtth' (MTTH myears mmonths mdays modifiers) = do
            modifiers_pp'd <- intersperse line <$> mapM pp_mtthmod modifiers
            let hasYears = isJust myears
                hasMonths = isJust mmonths
                hasDays = isJust mdays
                hasModifiers = not (null modifiers)
            return . mconcat $
                case myears of
                    Just years ->
                        [PP.int years, space, strictText $ plural years "year" "years"]
                        ++
                        if hasMonths && hasDays then [",", space]
                        else if hasMonths || hasDays then ["and", space]
                        else []
                    Nothing -> []
                ++
                case mmonths of
                    Just months ->
                        [PP.int months, space, strictText $ plural months "month" "months"]
                    _ -> []
                ++
                case mdays of
                    Just days ->
                        (if hasYears && hasMonths then ["and", space]
                         else []) -- if years but no months, already added "and"
                        ++
                        [PP.int days, space, strictText $ plural days "day" "days"]
                    _ -> []
                ++
                (if hasModifiers then
                    [line, "<br/>'''Modifiers'''", line]
                    ++ modifiers_pp'd
                 else [])
        pp_mtthmod (MTTHModifier (Just factor) conditions) =
            case conditions of
                [_] -> do
                    conditions_pp'd <- pp_script conditions
                    return . mconcat $
                        [conditions_pp'd
                        ,enclose ": '''×" "'''" (pp_float factor)
                        ]
                _ -> do
                    conditions_pp'd <- indentUp (pp_script conditions)
                    return . mconcat $
                        ["*"
                        ,enclose "'''×" "''':" (pp_float factor)
                        ,line
                        ,conditions_pp'd
                        ]
        pp_mtthmod (MTTHModifier Nothing _)
            = return "(invalid modifier! Bug in extractor?)"

--------------------------------
-- General statement handlers --
--------------------------------

compound :: Text -> GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
compound header (Statement _ (CompoundRhs scr))
    = withCurrentIndent $ \_ -> do -- force indent level at least 1
        headerMsg <- plainMsg (header <> ":")
        scriptMsgs <- ppMany scr
        return $ headerMsg ++ scriptMsgs
compound _ stmt = preStatement stmt

compoundMessage :: ScriptMessage -> GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
compoundMessage header (Statement _ (CompoundRhs scr))
    = withCurrentIndent $ \i -> do
        script_pp'd <- ppMany scr
        return ((i, header) : script_pp'd)
compoundMessage _ stmt = preStatement $ stmt

-- RHS is a localizable atom.
withLocAtom :: (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtom msg (Statement _ rhs) | Just key <- textRhs rhs
    = msgToPP =<< msg <$> getGameL10n key
withLocAtom _ stmt = preStatement stmt

-- RHS is a localizable atom and we need a second one (passed to message as
-- first arg).
withLocAtom2 :: ScriptMessage -> (Text -> Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtom2 inMsg msg (Statement _ rhs) | Just key <- textRhs rhs
    = msgToPP =<< msg <$> messageText inMsg <*> getGameL10n key
withLocAtom2 _ _ stmt = preStatement stmt

withLocAtomAndIcon :: Text -> (Text -> Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtomAndIcon iconkey msg (Statement _ rhs) | Just key <- textRhs rhs
    = do what <- getGameL10n key
         msgToPP $ msg (iconText iconkey) what
withLocAtomAndIcon _ _ stmt = preStatement stmt

withLocAtomIcon :: (Text -> Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withLocAtomIcon msg stmt@(Statement _ rhs) | Just key <- textRhs rhs
    = withLocAtomAndIcon key msg stmt
withLocAtomIcon _ stmt = preStatement stmt

withProvince :: (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withProvince msg (Statement lhs (floatRhs -> Just id))
    = withLocAtom msg (Statement lhs (GenericRhs (T.pack ("PROV" <> show (id::Int)))))
withProvince _ stmt = preStatement stmt

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
    ,("navy_reformer", "naval reformer") -- these are both used!
    ,("army_organizer", "army organizer")
    ,("army_reformer", "army reformer")
    ,("grand_captain", "grand captain")
    ,("master_recruiter", "master recruiter")
    ,("military_engineer", "military engineer")
    ,("spy_ideas", "espionage")
    ,("economic_ideas", "economic")
    ,("trade_ideas", "trade")
    ,("administrative_ideas", "administrative")
    ,("innovativeness_ideas", "innovative")
    ,("aristocracy_ideas", "aristocratic")
    ,("religious_ideas", "religious")
    ,("diplomatic_ideas", "diplomatic")
    ,("influence_ideas", "influence")
    ,("estate_church", "clergy")
    ,("estate_nobles", "nobles")
    ,("estate_burghers", "burghers")
    ,("estate_cossacks", "cossacks")
    ,("estate_nomadic_tribes", "tribes")
    ,("estate_dhimmi", "dhimmi")
    ,("base production", "production")
    ,("particularist", "particularists")
    ,("is_monarch_leader", "ruler general")
    ,("piety", "being pious") -- chosen arbitrarily
    ,("nomad_group", "nomadic")
    ,("tengri_pagan_reformed", "tengri")
    ,("norse_pagan_reformed", "norse")
    ,("mesoamerican_religion", "mayan")
    ]

-- Given a script atom, return the corresponding icon key, if any.
iconKey :: Text -> Maybe Text
iconKey atom = HM.lookup atom scriptIconTable

iconFileTable :: HashMap Text Text
iconFileTable = HM.fromList
    [("global tax modifier", "National tax modifier")
    ,("stability cost", "Stability cost modifier")
    ,("land maintenance", "Land maintenance modifier")
    ,("tolerance of the true faith", "tolerance own")
    ,("light ship combat ability", "light ship power")
    ]

-- Given an {{icon}} key, give the corresponding icon file name.
--
-- Needed for idea groups, which don't use {{icon}}.
iconFile :: Text -> Text
iconFile s = HM.lookupDefault s s iconFileTable
-- ByteString version
iconFileB :: ByteString -> ByteString
iconFileB = TE.encodeUtf8 . iconFile . TE.decodeUtf8

-- As generic_icon except
-- * say "same as <foo>" if foo refers to a country (in which case, add a flag)
-- * may not actually have an icon (localization file will know if it doesn't)
iconOrFlag :: (Text -> Text -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
iconOrFlag iconmsg flagmsg (Statement (GenericLhs _) (GenericRhs name)) = msgToPP =<< do
    nflag <- flag name -- laziness means this might not get evaluated
    if isTag name || isPronoun name
        then return . flagmsg . doc2text $ nflag
        else iconmsg <$> return (iconText . HM.lookupDefault name name $ scriptIconTable)
                     <*> getGameL10n name
iconOrFlag _ _ stmt = plainMsg $ pre_statement' stmt

tagOrProvince :: (Text -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
tagOrProvince tagmsg provmsg stmt@(Statement _ (floatOrTextRhs -> eobject))
    = msgToPP =<< case eobject of
            Just (Right tag) -> do -- is a tag
                tagflag <- flag tag
                return . tagmsg . doc2text $ tagflag
            Just (Left provid) -> do -- is a province id
                prov_loc <- getProvLoc provid
                return . provmsg $ prov_loc
            Nothing -> return (preMessage stmt)
tagOrProvince _ _ stmt = preStatement stmt

-- Numeric statement.
numeric :: (Double -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
numeric msg (Statement _ rhs) 
    | Just n <- floatRhs rhs = msgToPP $ msg n
numeric _ stmt = plainMsg $ pre_statement' stmt

numericOrTag :: (Double -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
numericOrTag numMsg tagMsg stmt@(Statement _ rhs) = msgToPP =<<
    case floatRhs rhs of
        Just n -> return $ numMsg n
        Nothing -> case textRhs rhs of
            Just t -> do -- assume it's a country
                tflag <- flag t
                return $ tagMsg (doc2text tflag)
            Nothing -> return (preMessage stmt)
numericOrTag _ _ stmt = preStatement stmt

-- Generic statement referring to a country. Use a flag.
withFlag :: (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withFlag msg (Statement _ (GenericRhs who)) = msgToPP =<< do
    whoflag <- flag who
    return . msg . doc2text $ whoflag
withFlag _ stmt = plainMsg $ pre_statement' stmt

withBool :: (Bool -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (preStatement stmt)
          return
          fullmsg

withBool' :: (Bool -> ScriptMessage) -> GenericStatement -> PP extra (Maybe [(Int, ScriptMessage)])
withBool' msg (Statement _ rhs) | Just yn <- textRhs rhs, T.map toLower yn `elem` ["yes","no","false"]
    = fmap Just . msgToPP $ case T.toCaseFold yn of
        "yes" -> msg True
        "no"  -> msg False
        "false" -> msg False
        _     -> error "impossible: withBool matched a string that wasn't yes, no or false"
withBool' _ _ = return Nothing

-- Statement may have "yes"/"no" or a tag.
withFlagOrBool :: (Bool -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
withFlagOrBool bmsg _ (Statement _ (GenericRhs "yes")) = msgToPP (bmsg True)
withFlagOrBool bmsg _ (Statement _ (GenericRhs "no"))  = msgToPP (bmsg False)
withFlagOrBool _ tmsg stmt = withFlag tmsg stmt

numericIcon :: Text -> (Text -> Double -> ScriptMessage) -> GenericStatement -> PP extra [(Int, ScriptMessage)]
numericIcon the_icon msg (Statement _ rhs) | Just amt <- floatRhs rhs
    = msgToPP $ msg (iconText the_icon) amt
numericIcon _ _ stmt = plainMsg $ pre_statement' stmt

----------------------
-- Text/value pairs --
----------------------

-- This is for statements of the form
--      head = {
--          what = some_atom
--          value = 3
--      }
-- e.g.
--      num_of_religion = {
--          religion = catholic
--          value = 0.5
--      }
-- There are several statements of this form, but with different "what" labels,
-- so the first parameter says what that label is. There's also one for value,
-- in case there are other labels for that.
--
-- There are two message parameters, one for value < 1 and one for value >= 1.
-- In the example num_of_religion, value is interpreted as a percentage of
-- provinces if less than 1, or a number of provinces otherwise. These require
-- rather different messages.
--
-- We additionally attempt to localize the RHS of "what". If it has no
-- localization string, it gets wrapped in a tt element instead.

-- convenience synonym
tryLoc :: Text -> PP extra (Maybe Text)
tryLoc = getGameL10nIfPresent

data TextValue = TextValue
        {   tv_what :: Maybe Text
        ,   tv_value :: Maybe Double
        }
newTV = TextValue Nothing Nothing
textValue :: forall extra.
             Text -- ^ Label for "what"
          -> Text -- ^ Label for "how much"
          -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
          -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
          -> (Text -> PP extra (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
          -> GenericStatement -> PP extra [(Int, ScriptMessage)]
textValue whatlabel vallabel smallmsg bigmsg loc stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_tv $ foldl' addLine newTV scr)
    where
        addLine :: TextValue -> GenericStatement -> TextValue
        addLine tv (Statement (GenericLhs label) rhs)
            | label == whatlabel, Just what <- textRhs rhs
            = tv { tv_what = Just what }
        addLine tv (Statement (GenericLhs label) rhs)
            | label == vallabel, Just val <- floatRhs rhs
            = tv { tv_value = Just val }
        addLine nor _ = nor
        pp_tv :: TextValue -> PP extra ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv) of
            (Just what, Just value) -> do
                mwhat_loc <- loc what
                let what_icon = iconText what
                    what_loc = maybe ("<tt>" <> what <> "</tt>") id mwhat_loc
                return $ (if abs value < 1 then smallmsg else bigmsg) what_icon what_loc value
            _ -> return $ preMessage stmt
textValue _ _ _ _ _ stmt = preStatement stmt

-- | Statements of the form
--      has_trade_modifier = {
--          who = ROOT
--          name = merchant_recalled
--      }
data TextAtom = TextAtom
        {   ta_what :: Maybe Text
        ,   ta_atom :: Maybe Text
        }
newTA = TextAtom Nothing Nothing
textAtom :: forall extra.
            Text -- ^ Label for "what"
         -> Text -- ^ Label for atom
         -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
         -> (Text -> PP extra (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
         -> GenericStatement -> PP extra [(Int, ScriptMessage)]
textAtom whatlabel atomlabel msg loc stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_tv $ foldl' addLine newTA scr)
    where
        addLine :: TextAtom -> GenericStatement -> TextAtom
        addLine ta (Statement (GenericLhs label) rhs)
            | label == whatlabel, Just what <- textRhs rhs
            = ta { ta_what = Just what }
        addLine ta (Statement (GenericLhs label) rhs)
            | label == atomlabel, Just at <- textRhs rhs
            = ta { ta_atom = Just at }
        addLine nor _ = nor
        pp_tv :: TextAtom -> PP extra ScriptMessage
        pp_tv ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_icon = iconText what
                    what_loc = maybe ("<tt>" <> what <> "</tt>") id mwhat_loc
                return $ msg what_icon what_loc atom_loc
            _ -> return $ preMessage stmt
textAtom _ _ _ _ stmt = preStatement stmt

gainIcon :: Text
            -> (Text -> Double -> ScriptMessage)
            -> GenericStatement
            -> PP extra [(Int, ScriptMessage)]
gainIcon iconkey msg
    (Statement _ rhs) | Just n <- floatRhs rhs
    = msgToPP $ msg (iconText iconkey) n
gainIcon _ _ stmt = preStatement stmt

gain' :: (Double -> ScriptMessage)
              -> GenericStatement
              -> PP extra [(Int, ScriptMessage)]
gain' msg
    (Statement _ rhs) | Just n <- floatRhs rhs
    = msgToPP $ msg n
gain' _ stmt = preStatement stmt

-- AI decision factors
-- Most of the code for this is in EU4.SuperCommon and re-exported here,
-- because EU4.IdeaGroups needs them. But only EU4.Common needs output
-- functions.
ppAiWillDo :: AIWillDo -> PP IdeaTable [(Int, ScriptMessage)]
ppAiWillDo (AIWillDo mbase mods) = do
    mods_pp'd <- fold <$> traverse ppAiMod mods
    let baseWtMsg = case mbase of
            Nothing -> MsgNoBaseWeight
            Just base -> MsgAIBaseWeight base
    iBaseWtMsg <- msgToPP baseWtMsg
    return $ iBaseWtMsg ++ mods_pp'd

ppAiMod :: AIModifier -> PP IdeaTable [(Int, ScriptMessage)]
ppAiMod (AIModifier (Just multiplier) triggers) = do
    triggers_pp'd <- ppMany triggers
    case triggers_pp'd of
        [(i, triggerMsg)] -> do
            triggerText <- messageText triggerMsg
            return [(i, MsgAIFactorOneline triggerText multiplier)]
        _ -> withCurrentIndentZero $ \i -> return $
            (i, MsgAIFactorHeader multiplier)
            : map (first succ) triggers_pp'd -- indent up
ppAiMod (AIModifier Nothing _) =
    plainMsg "(missing multiplier for this factor)"

---------------------------------
-- Specific statement handlers --
---------------------------------

-- Factions.
-- We want to use the faction influence icons, not the faction icons, so
-- textValue unfortunately doesn't work here.

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
                            "enuchs" {- sic -} -> Just "eunuchs influence"
                            "temples"          -> Just "temples influence"
                            "bureaucrats"      -> Just "bureaucrats influence"
                            -- Merchant republic
                            "mr_aristocrats"   -> Just "aristocrats influence"
                            "mr_guilds"        -> Just "guilds influence"
                            "mr_traders"       -> Just "traders influence"
                            _ {- unknown -}    -> Nothing
                    fac_icon = maybe ("<!-- " <> fac <> " -->") iconText fac_iconkey
                    infl = fromJust (influence inf)
                in do
                    fac_loc <- getGameL10n fac
                    return $ MsgFactionGainInfluence fac_icon fac_loc infl
            else return $ preMessage stmt
        addField :: FactionInfluence -> GenericStatement -> FactionInfluence
        addField inf (Statement (GenericLhs "faction") (GenericRhs fac)) = inf { faction = Just fac }
        addField inf (Statement (GenericLhs "influence") rhs) | Just amt <- floatRhs rhs = inf { influence = Just amt }
        addField inf _ = inf -- unknown statement
factionInfluence stmt = preStatement stmt

-- Modifiers

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
            (Just who, Just name, _,        Nothing,  Nothing)  -> return (MsgActorGainsMod who kind name)
            (Just who, Nothing,   Just key, Nothing,  Nothing)  -> return (MsgActorGainsMod who kind key)
            (Just who, Just name, _,        Nothing,  Just dur) -> return (MsgActorGainsModDur who kind name dur)
            (Just who, Nothing,   Just key, Nothing,  Just dur) -> return (MsgActorGainsModDur who kind key dur)
            (Just who, Just name, _,        Just pow, Nothing)  -> return (MsgActorGainsModPow who kind name pow)
            (Just who, Nothing,   Just key, Just pow, Nothing)  -> return (MsgActorGainsModPow who kind key pow)
            (Just who, Just name, _,        Just pow, Just dur) -> return (MsgActorGainsModPowDur who kind name pow dur)
            (Just who, Nothing,   Just key, Just pow, Just dur) -> return (MsgActorGainsModPowDur who kind key pow dur)
    else return (preMessage stmt)
addModifier _ stmt = preStatement stmt

-- Add core

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

-- Opinions

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
        addLine op (Statement (GenericLhs "modifier") rhs) | Just label <- textRhs rhs
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
              else trace ("failed! modifier op is " ++ show (modifier op)) $ return (preMessage stmt)
opinion _ _ stmt = preStatement stmt

data HasOpinion = HasOpinion
        {   hop_who :: Maybe Text
        ,   hop_value :: Maybe Double
        }
newHasOpinion = HasOpinion Nothing Nothing
hasOpinion :: GenericStatement -> PP extra [(Int, ScriptMessage)]
hasOpinion stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_hasOpinion $ foldl' addLine newHasOpinion scr)
    where
        addLine :: HasOpinion -> GenericStatement -> HasOpinion
        addLine hop (Statement (GenericLhs "who")   rhs) | Just who <- textRhs  rhs = hop { hop_who = Just who }
        addLine hop (Statement (GenericLhs "value") rhs) | Just val <- floatRhs rhs = hop { hop_value = Just val }
        addLine hop _ = trace ("warning: unrecognized has_opinion clause") hop
        pp_hasOpinion :: HasOpinion -> PP extra ScriptMessage
        pp_hasOpinion hop = case (hop_who hop, hop_value hop) of
            (Just who, Just value) -> do
                who_flag <- flag who
                return (MsgHasOpinion value (doc2text who_flag))
            _ -> return (preMessage stmt)
hasOpinion stmt = preStatement stmt

-- Rebels

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
    spawnRebels' Nothing (Statement _ (CompoundRhs scr))
        = pp_spawnRebels $ foldl' addLine newSpawnRebels scr
    spawnRebels' rtype (Statement _ rhs) | Just size <- floatRhs rhs
        = pp_spawnRebels $ newSpawnRebels { rebelType = rtype, rebelSize = Just size }
    spawnRebels' _ stmt = return (preMessage stmt)

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

-- Events

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
        addLine evt (Statement (GenericLhs "days") rhs)
            = return evt { e_days = floatRhs rhs }
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
triggerEvent _ stmt = preStatement stmt

-- Specific values

gainManpower :: GenericStatement -> PP extra [(Int, ScriptMessage)]
gainManpower (Statement _ rhs) | Just amt <- floatRhs rhs = msgToPP =<<
    let mpicon = iconText "manpower"
    in if abs (amt::Double) < 1
        --  interpret amt as a fraction of max
        then return $ MsgGainMPFrac mpicon amt
        --  interpret amt as a multiple of 1,000
        else return $ MsgGainMP mpicon (amt*1000)
gainManpower stmt = preStatement stmt

-- Casus belli

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
            = return $ acb { acb_months = floatRhs rhs }
        addLine acb _ = return acb
        pp_add_cb :: AddCB -> ScriptMessage
        pp_add_cb acb =
            let msg = if direct then MsgGainCB else MsgReverseGainCB
                msg_dur = if direct then MsgGainCBDuration else MsgReverseGainCBDuration
            in case (acb_type acb, acb_type_loc acb,
                     acb_target_flag acb,
                     acb_months acb) of
                (Nothing, _, _, _) -> preMessage stmt -- need CB type
                (_, _, Nothing, _) -> preMessage stmt -- need target
                (_, Just cbtype_loc, Just target_flag, Just months) -> msg_dur cbtype_loc target_flag months
                (Just cbtype, Nothing, Just target_flag, Just months) -> msg_dur cbtype target_flag months
                (_, Just cbtype_loc, Just target_flag, Nothing) -> msg cbtype_loc target_flag
                (Just cbtype, Nothing, Just target_flag, Nothing) -> msg cbtype target_flag
addCB _ stmt = preStatement stmt

-- Random

random :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
random stmt@(Statement _ (CompoundRhs scr))
    | (front, back) <- break
                        (\stmt -> case stmt of 
                            Statement (GenericLhs "chance") _ -> True
                            _ -> False)
                        scr
      , not (null back)
      , Statement _ rhs <- head back
      , Just chance <- floatRhs rhs
      = compoundMessage
          (MsgRandomChance chance)
          (Statement undefined (CompoundRhs (front ++ tail back)))
    | otherwise = compoundMessage MsgRandom stmt
random stmt = preStatement stmt

-- Advisors

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
        addLine da (Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
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
            _ -> return da
        addLine da _ = return da
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
                           da_type_loc da,
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
defineAdvisor stmt = preStatement stmt

-- Rulers

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
defineRuler (Statement _ (CompoundRhs scr))
    = pp_define_ruler $ foldl' addLine newDefineRuler scr where
        addLine :: DefineRuler -> GenericStatement -> DefineRuler
        addLine dr (Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
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
            _ -> dr
        addLine dr _ = dr
        pp_define_ruler :: DefineRuler -> PP extra [(Int, ScriptMessage)]
        pp_define_ruler    DefineRuler { dr_rebel = True } = msgToPP MsgRebelLeaderRuler
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

-- Building units

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
        addLine dr (Statement (GenericLhs lhs) rhs)
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
        addLine dr _ = dr
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

-- War

data DeclareWarWithCB = DeclareWarWithCB
    {   dwcb_who :: Maybe Text
    ,   dwcb_cb :: Maybe Text
    }
newDeclareWarWithCB = DeclareWarWithCB Nothing Nothing

declareWarWithCB :: GenericStatement -> PP extra [(Int, ScriptMessage)]
declareWarWithCB stmt@(Statement _ (CompoundRhs scr))
    = msgToPP =<< (pp_declare_war_with_cb $ foldl' addLine newDeclareWarWithCB scr) where
        addLine :: DeclareWarWithCB -> GenericStatement -> DeclareWarWithCB
        addLine dwcb (Statement (GenericLhs lhs) (GenericRhs rhs))
            = case T.map toLower lhs of
                "who"         -> dwcb { dwcb_who = Just rhs }
                "casus_belli" -> dwcb { dwcb_cb  = Just rhs }
                _ -> dwcb
        addLine dwcb _ = dwcb
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

-- DLC

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

-- Estates

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

-- Trigger switch

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

-- Heirs

data Heir = Heir
        {   heir_dynasty :: Maybe Text
        ,   heir_claim :: Maybe Double
        ,   heir_age :: Maybe Double
        }
newHeir = Heir Nothing Nothing Nothing
defineHeir :: GenericStatement -> PP extra [(Int, ScriptMessage)]
defineHeir (Statement _ (CompoundRhs scr))
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

-- Holy Roman Empire

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
    _ -> error "called hreReformLoc with n < 1 or n > 8"

hreReformLevel :: GenericStatement -> PP extra [(Int, ScriptMessage)]
hreReformLevel (Statement _ rhs) | Just level <- floatRhs rhs, level >= 0, level <= 8
    = if level == 0
        then msgToPP MsgNoHREReforms
        else msgToPP . MsgHREPassedReform =<< hreReformLoc level
hreReformLevel stmt = preStatement stmt

-- Religion

religionYears :: GenericStatement -> PP extra [(Int, ScriptMessage)]
religionYears (Statement _ (CompoundRhs [Statement (GenericLhs rel) rhs]))
    | Just years <- floatRhs rhs = do
        let rel_icon = iconText rel
        rel_loc <- getGameL10n rel
        msgToPP $ MsgReligionYears rel_icon rel_loc years
religionYears stmt = preStatement stmt

-- Government

govtRank :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
govtRank (Statement _ rhs) | Just level <- floatRhs rhs
    = case level :: Int of
        1 -> msgToPP MsgRankDuchy -- unlikely, but account for it anyway
        2 -> msgToPP MsgRankKingdom
        3 -> msgToPP MsgRankEmpire
        _ -> error "impossible: govtRank matched an invalid rank number"
govtRank stmt = preStatement stmt

setGovtRank :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
setGovtRank (Statement _ rhs) | Just level <- floatRhs rhs, level `elem` [1..3]
    = case level :: Int of
        1 -> msgToPP MsgSetRankDuchy
        2 -> msgToPP MsgSetRankKingdom
        3 -> msgToPP MsgSetRankEmpire
        _ -> error "impossible: setGovtRank matched an invalid rank number"
setGovtRank stmt = preStatement stmt

numProvinces :: Text -> (Text -> Text -> Double -> ScriptMessage) -> GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
numProvinces iconKey msg (Statement (GenericLhs what) rhs)
    | Just amt <- floatRhs rhs = do
        what_loc <- getGameL10n what
        msgToPP (msg (iconText iconKey) what_loc amt)
numProvinces _ _ stmt = preStatement stmt

withFlagOrProvince :: (Text -> ScriptMessage) -> (Text -> ScriptMessage) -> GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
withFlagOrProvince countryMsg provinceMsg stmt@(Statement _ rhs)
    | Just _ <- textRhs rhs = withFlag countryMsg stmt
    | Just _ <- floatRhs rhs :: Maybe Double
                            = withProvince provinceMsg stmt
withFlagOrProvince _ _ stmt = preStatement stmt

tradeMod :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
tradeMod stmt@(Statement _ rhs)
    | Just _ <- textRhs rhs = withLocAtom2 MsgTradeMod MsgHasModifier stmt
    | CompoundRhs _ <- rhs  = textAtom "who" "name" MsgHasTradeModifier (fmap Just . flagText) stmt
tradeMod stmt = preStatement stmt

isMonth :: GenericStatement -> PP IdeaTable [(Int, ScriptMessage)]
isMonth (Statement _ rhs) | Just num <- floatRhs rhs, (num::Int) >= 1, num <= 12
    = do
        month_loc <- getGameL10n $ case num of
            1 -> "January"
            2 -> "February"
            3 -> "March"
            4 -> "April"
            5 -> "May"
            6 -> "June"
            7 -> "July"
            8 -> "August"
            9 -> "September"
            10 -> "October"
            11 -> "November"
            12 -> "December"
            _ -> error "impossible: tried to localize bad month number"
        msgToPP $ MsgIsMonth month_loc
isMonth stmt = preStatement stmt

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
hasIdea _ stmt = preStatement stmt
