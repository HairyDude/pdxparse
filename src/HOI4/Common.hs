{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}
{-|
Module      : HOI4.Common
Description : Statement handlers for Hearts of Iron IV
-}
module HOI4.Common (
        pp_script
    ,   pp_mtth
    ,   ppOne
    ,   ppMany
    ,   iconKey
--  ,   AIWillDo (..), AIModifier (..)
    ,   HOI4Scope (..)
--  ,   ppAiWillDo, ppAiMod
    ,   module HOI4.Types
    ) where

import Debug.Trace (trace)

import Control.Applicative (liftA2)
import Control.Monad (mapM, forM, sequence, join, foldM)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..), gets)

import Data.Char (isUpper, toUpper, toLower)
import Data.List (intersperse, foldl')
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Monoid ((<>))

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

-- TODO: get rid of these, do icon key lookups from another module
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Trie (Trie)
import qualified Data.Trie as Tr

import qualified Data.Set as S

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import Messages -- everything
import MessageTools (plural)
import QQ (pdx)
import SettingsTypes ( PPT, GameState (..), Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nDefault, getGameL10nIfPresent
                     , withCurrentIndent, indentUp, indentDown, alsoIndent'
                     , withCurrentFile)
import HOI4.Types -- everything

-- no particular order from here... TODO: organize this!

-- | Convert a single (possibly compound) statement into a (series of) indented
-- message(s).
msgToPP :: (IsGameState (GameState g), Monad m) => ScriptMessage -> PPT g m IndentedMessages
msgToPP msg = (:[]) <$> alsoIndent' msg

-- | Does the given text look like a country tag? I.e. does it consist of 3
-- upper-case letters?
isTag :: Text -> Bool
isTag s = T.length s == 3 && T.all isUpper s

-- | Is the given text a "prounoun" (e.g. ROOT)?
isPronoun :: Text -> Bool
isPronoun s = T.map toLower s `S.member` pronouns where
    pronouns = S.fromList
        ["root"
        ,"prev"
        ,"owner"
        ,"controller"
        ]

-- | Format a script as wiki text.
pp_script :: (HOI4Info g, Monad m) => GenericScript -> PPT g m Doc
pp_script [] = return "(Nothing)"
pp_script script = imsg2doc_html =<< ppMany script

-- | Emit @{{flag}}@ template if the argument looks like a tag.
flag :: (IsGameData (GameData g), Monad m) => Text -> PPT g m Doc
flag name =
    if isTag name
        then template "flag" . (:[]) <$> getGameL10n name
        else return $ case T.map toUpper name of
                "ROOT" -> "(Our country)" -- will need editing for capitalization in some cases
                "PREV" -> "(Previously mentioned country)"
                -- Suggestions of a description for FROM are welcome.
                _ -> Doc.strictText name

-- Emit icon template.
icon :: Text -> Doc
icon what = template "icon" [HM.lookupDefault what what scriptIconTable, "28px"]
iconText :: Text -> Text
iconText = Doc.doc2text . icon

-- | Create a generic message from a piece of text. The rendering function will
-- pass this through unaltered.
plainMsg :: (IsGameState (GameState g), Monad m) =>
    Text -> PPT g m IndentedMessages
plainMsg msg = (:[]) <$> (alsoIndent' . MsgUnprocessed $ msg)

-- | Pretty-print a statement and wrap it in a @<pre>@ element.
pre_statement :: GenericStatement -> Doc
pre_statement stmt = "<pre>" <> genericStatement2doc stmt <> "</pre>"

-- | 'Text' version of 'pre_statement'.
pre_statement' :: GenericStatement -> Text
pre_statement' = Doc.doc2text . pre_statement

-- | Pretty-print a script statement, wrap it in a @<pre>@ element, and emit a
-- generic message for it.
preMessage :: GenericStatement -> ScriptMessage
preMessage = MsgUnprocessed
            . TL.toStrict
            . PP.displayT
            . PP.renderPretty 0.8 80 -- Don't use 'Doc.doc2text', because it uses
                                     -- 'Doc.renderCompact' which is not what
                                     -- we want here.
            . pre_statement

-- | Pretty-print a script statement, wrap it in a @<pre>@ element, and emit a
-- generic message for it at the current indentation level. This is the
-- fallback in case we haven't implemented that particular statement or we
-- failed to understand it.
preStatement :: (IsGameState (GameState g), Monad m) => GenericStatement -> PPT g m IndentedMessages
preStatement stmt = (:[]) <$> alsoIndent' (preMessage stmt)

-- | Extract the appropriate message(s) from a script.
ppMany :: (HOI4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppMany scr = indentUp (concat <$> mapM ppOne scr)

-- | Convenience synonym.
type StatementHandler g m = GenericStatement -> PPT g m IndentedMessages

-- Table of handlers for statements.
-- Dispatch on strings is /much/ quicker using a lookup table than a
-- huge case statement, which uses (==) on each one in turn.
ppHandlers :: (HOI4Info g, Monad m) => Trie (StatementHandler g m)
ppHandlers = Tr.fromList
        -- Statements where RHS is irrelevant (usually "yes")
        [
--      ,("cancel_construction"    , const (msgToPP MsgCancelConstruction)) -- Canals
        -- Numbers
--      ,("is_year"                          , numeric MsgYearIs)
        -- ... with icons
--      ,("create_admiral"           , numericIcon "admiral" MsgCreateAdmiral)
        -- Used in ideas and other bonuses, omit "gain/lose" in l10n
--      ,("build_cost"                        , numericIcon "build cost" MsgBuildCost)
--      ,("production_efficiency"             , numericIcon "production efficiency" MsgProdEff)
        -- Modifiers
--      ,("add_country_modifier"           , addModifier MsgCountryMod)
        -- Simple compound statements
        -- Note that "any" can mean "all" or "one or more" depending on context.
         ("and" , compoundMessage MsgAllOf)
        ,("root", compoundMessage MsgOurCountry)
        -- These two are ugly, but without further analysis we can't know
        -- what it means.
        ,("from", compoundMessage MsgFROM)
        ,("prev", compoundMessage MsgPREV)
        ,("not" , compoundMessage MsgNoneOf)
        ,("or"  , compoundMessage MsgAtLeastOneOf)
        -- There is a semantic distinction between "all" and "every",
        -- namely that the former means "this is true for all <type>" while
        -- the latter means "do this for every <type>."
        ,("all_country" {- sic -}   , scope HOI4Country   . compoundMessage MsgAllCountries)
        ,("any_country"             , scope HOI4Country   . compoundMessage MsgAnyCountry)
        ,("any_enemy_country"       , scope HOI4Country   . compoundMessage MsgAnyEnemyCountry)
        ,("capital_scope"           , scope HOI4Province  . compoundMessage MsgCapital)
        ,("controller"              , scope HOI4Country   . compoundMessage MsgController)
        ,("every_country"           , scope HOI4Country   . compoundMessage MsgEveryCountry)
        ,("hidden_effect"           ,                       compoundMessage MsgHiddenEffect)
        ,("if"                      ,                       compoundMessage MsgIf) -- always needs editing
        ,("limit"                   ,                       compoundMessage MsgLimit) -- always needs editing
        ,("owner"                   , scope HOI4Country   . compoundMessage MsgOwner)
        ,("random_country"          , scope HOI4Country   . compoundMessage MsgRandomCountry)
        ,("random_list"             ,                       compoundMessage MsgRandom)
        ,("random_owned_province"   , scope HOI4Province  . compoundMessage MsgRandomOwnedProvince)
        -- Random
        ,("random", random)
        -- Simple generic statements (RHS is a localizable atom)
        ,("region"                , withLocAtom MsgRegionIs)
        -- RHS is a province ID
        ,("capital"           , withProvince MsgCapitalIs)
        ,("set_capital"       , withProvince MsgSetCapital)
        -- RHS is a flag OR a province ID
--      ,("remove_core"        , withFlagOrProvince MsgLoseCoreCountry MsgLoseCoreProvince)
        -- RHS is a flag or province id, but the statement's meaning depends on the scope
--      ,("has_discovered"     , withFlagOrProvinceHOI4Scope MsgHasDiscovered MsgDiscoveredBy) -- scope sensitive
        -- Simple generic statements (typewriter face)
        ,("clr_country_flag" , withNonlocAtom2 MsgCountryFlag MsgClearFlag)
        ,("clr_global_flag"  , withNonlocAtom2 MsgGlobalFlag MsgClearFlag)
        ,("has_country_flag" , withNonlocAtom2 MsgCountryFlag MsgHasFlag)
        ,("has_global_flag"  , withNonlocAtom2 MsgGlobalFlag MsgHasFlag)
        ,("set_country_flag" , withNonlocAtom2 MsgCountryFlag MsgSetFlag)
        ,("set_global_flag"  , withNonlocAtom2 MsgGlobalFlag MsgSetFlag)
        -- Simple generic statements with icon
--      ,("is_monarch_leader"       , withLocAtomAndIcon "ruler general" MsgRulerIsGeneral)
        -- Simple generic statements with flag
        ,("is_controlled_by"        , withFlag MsgControlledBy)
        ,("has_defensive_war_with"  , withFlag MsgDefensiveWarAgainst)
        ,("add_claim_by"            , withFlag MsgGainClaim)
        ,("has_offensive_war_with"  , withFlag MsgOffensiveWarAgainst)
        ,("overlord_of"             , withFlag MsgOverlordOf)
        ,("s_owned_by"              , withFlag MsgOwnedBy)
        ,("tag"                     , withFlag MsgCountryIs)
        ,("truce_with"              , withFlag MsgTruceWith)
        ,("has_war_with"            , withFlag MsgAtWarWith)
        ,("white_peace"             , withFlag MsgMakeWhitePeace)
        -- Simple generic statements with flag or "yes"/"no"
        ,("exists", withFlagOrBool MsgExists MsgCountryExists) -- XXX
        ,("country_exists", withFlagOrBool MsgExists MsgCountryExists) -- XXX
        -- Statements that may be an icon, a flag, or a pronoun (such as ROOT)
        -- Boolean argument is whether to emit an icon.
--      ,("change_religion", iconOrFlag MsgChangeReligion MsgChangeSameReligion)
        -- Statements that may be either a tag or a province
--      ,("is_core" , tagOrProvince MsgIsCoreOf MsgHasCoreOn)
--      ,("is_claim", tagOrProvince MsgHasClaim MsgHasClaimOn)
        -- Boolean statements
        ,("is_ai"                       , withBool MsgIsAIControlled)
        ,("always"                      , withBool MsgAlways)
        -- Statements that may be numeric or a tag
--      ,("num_of_cities", numericOrTag MsgNumCities MsgNumCitiesThan)
        -- Signed numeric statements
--      ,("tolerance_to_this", numeric MsgToleranceToThis)
        -- Special cases
--      ,("legitimacy_or_horde_unity", numeric MsgLegitimacyOrHordeUnity)
        -- Number of provinces of some kind, mostly religions and trade goods
--      ,("wool"          , numProvinces "wool" MsgGoodsProvinces)
        -- Special
--      ,("add_manpower"        , gainManpower) -- numbers are different
        ,("has_dlc"             , hasDlc)
        -- Special complex statements
        ,("country_event"                , scope HOI4Country . triggerEvent MsgCountryEvent)
--      ,("has_opinion"                  , hasOpinion)
--      ,("has_opinion_modifier"         , opinion MsgHasOpinionMod (\what who _years -> MsgHasOpinionMod what who))
--      ,("remove_opinion_modifier"      , opinion MsgRemoveOpinionMod (\what who _years -> MsgRemoveOpinionMod what who))
        -- Ignored
        ,("custom_tooltip", const (plainMsg "(custom tooltip - delete this line)"))
        ,("tooltip"       , const (plainMsg "(explanatory tooltip - delete this line)"))
        ]

ppOne :: (HOI4Info g, Monad m) => StatementHandler g m
ppOne stmt@[pdx| %lhs = %rhs |] = case lhs of
    GenericLhs label -> case Tr.lookup (TE.encodeUtf8 (T.toLower label)) ppHandlers of
        Just handler -> handler stmt
        -- default
        Nothing -> do
            mloc <- getGameL10nIfPresent label
            case mloc of
                -- Check for localizable atoms, e.g. regions
                Just loc -> compound loc stmt
                Nothing -> preStatement stmt
    AtLhs _ -> return [] -- don't know how to handle these
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

-----------------------------------------------------------------
-- Script handlers that should be used directly, not via ppOne --
-----------------------------------------------------------------

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
newMTTH :: MTTH
newMTTH = MTTH Nothing Nothing Nothing []
newMTTHMod :: MTTHModifier
newMTTHMod = MTTHModifier Nothing []
pp_mtth :: (HOI4Info g, Monad m) => GenericScript -> PPT g m Doc
pp_mtth = pp_mtth' . foldl' addField newMTTH
    where
        addField mtth [pdx| years    = !n   |] = mtth { mtth_years = Just n }
        addField mtth [pdx| months   = !n   |] = mtth { mtth_months = Just n }
        addField mtth [pdx| days     = !n   |] = mtth { mtth_days = Just n }
        addField mtth [pdx| modifier = @rhs |] = addMTTHMod mtth rhs
        addField mtth _ = mtth -- unrecognized
        addMTTHMod mtth scr = mtth {
                mtth_modifiers = mtth_modifiers mtth
                                 ++ [foldl' addMTTHModField newMTTHMod scr] } where
            addMTTHModField mtthmod [pdx| factor = !n |]
                = mtthmod { mtthmod_factor = Just n }
            addMTTHModField mtthmod stmt -- anything else is a condition
                = mtthmod { mtthmod_conditions = mtthmod_conditions mtthmod ++ [stmt] }
        pp_mtth' (MTTH myears mmonths mdays modifiers) = do
            modifiers_pp'd <- intersperse PP.line <$> mapM pp_mtthmod modifiers
            let hasYears = isJust myears
                hasMonths = isJust mmonths
                hasDays = isJust mdays
                hasModifiers = not (null modifiers)
            return . mconcat $
                case myears of
                    Just years ->
                        [PP.int years, PP.space, Doc.strictText $ plural years "year" "years"]
                        ++
                        if hasMonths && hasDays then [",", PP.space]
                        else if hasMonths || hasDays then ["and", PP.space]
                        else []
                    Nothing -> []
                ++
                case mmonths of
                    Just months ->
                        [PP.int months, PP.space, Doc.strictText $ plural months "month" "months"]
                    _ -> []
                ++
                case mdays of
                    Just days ->
                        (if hasYears && hasMonths then ["and", PP.space]
                         else []) -- if years but no months, already added "and"
                        ++
                        [PP.int days, PP.space, Doc.strictText $ plural days "day" "days"]
                    _ -> []
                ++
                (if hasModifiers then
                    [PP.line, "<br/>'''Modifiers'''", PP.line]
                    ++ modifiers_pp'd
                 else [])
        pp_mtthmod (MTTHModifier (Just factor) conditions) =
            case conditions of
                [_] -> do
                    conditions_pp'd <- pp_script conditions
                    return . mconcat $
                        [conditions_pp'd
                        ,PP.enclose ": '''×" "'''" (Doc.pp_float factor)
                        ]
                _ -> do
                    conditions_pp'd <- indentUp (pp_script conditions)
                    return . mconcat $
                        ["*"
                        ,PP.enclose "'''×" "''':" (Doc.pp_float factor)
                        ,PP.line
                        ,conditions_pp'd
                        ]
        pp_mtthmod (MTTHModifier Nothing _)
            = return "(invalid modifier! Bug in extractor?)"

--------------------------------
-- General statement handlers --
--------------------------------

compound :: (HOI4Info g, Monad m) => Text -> StatementHandler g m
compound header [pdx| %_ = @scr |]
    = withCurrentIndent $ \_ -> do -- force indent level at least 1
        headerMsg <- plainMsg (header <> ":")
        scriptMsgs <- ppMany scr
        return $ headerMsg ++ scriptMsgs
compound _ stmt = preStatement stmt

compoundMessage :: (HOI4Info g, Monad m) => ScriptMessage -> StatementHandler g m
compoundMessage header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        script_pp'd <- ppMany scr
        return ((i, header) : script_pp'd)
compoundMessage _ stmt = preStatement stmt

-- RHS is a localizable atom.
withLocAtom :: (IsGameData (GameData g),
                IsGameState (GameState g),
                Monad m) =>
    (Text -> ScriptMessage) ->  StatementHandler g m
withLocAtom msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> getGameL10n key
withLocAtom _ stmt = preStatement stmt

-- RHS is a localizable atom and we need a second one (passed to message as
-- first arg).
withLocAtom2 :: (IsGameData (GameData g),
                 IsGameState (GameState g),
                 Monad m) =>
    ScriptMessage
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtom2 inMsg msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> pure key <*> messageText inMsg <*> getGameL10n key
withLocAtom2 _ _ stmt = preStatement stmt

withLocAtomAndIcon :: (IsGameData (GameData g),
                       IsGameState (GameState g),
                       Monad m) =>
    Text
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomAndIcon iconkey msg [pdx| %_ = $key |]
    = do what <- getGameL10n key
         msgToPP $ msg (iconText iconkey) what
withLocAtomAndIcon _ _ stmt = preStatement stmt

withLocAtomIcon :: (IsGameData (GameData g),
                    IsGameState (GameState g),
                    Monad m) =>
    (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomIcon msg stmt@[pdx| %_ = $key |]
    = withLocAtomAndIcon (fromMaybe key (iconKey key)) msg stmt
withLocAtomIcon _ stmt = preStatement stmt

{-
withLocAtomIconHOI4Scope :: Monad m =>
    (Text -> Text -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> GenericStatement -> PPT g m IndentedMessages
withLocAtomIconHOI4Scope countrymsg provincemsg stmt = do
    thescope <- getCurrentHOI4Scope
    case thescope of
        Just HOI4Country -> withLocAtomIcon countrymsg stmt
        Just HOI4Province -> withLocAtomIcon provincemsg stmt
        _ -> preStatement stmt -- others don't make sense
-}

withProvince :: (IsGameData (GameData g),
                 IsGameState (GameState g),
                 Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withProvince msg [pdx| %lhs = !provid |]
    = withLocAtom msg [pdx| %lhs = $(T.pack ("PROV" <> show (provid::Int))) |]
withProvince _ stmt = preStatement stmt

-- As withLocAtom but no l10n.
-- Currently unused
--withNonlocAtom :: (Text -> ScriptMessage) -> GenericStatement -> PP extra IndentedMessages
--withNonlocAtom msg [pdx| %_ = ?text |] = msgToPP $ msg text
--withNonlocAtom _ stmt = preStatement stmt

-- As withNonlocAtom but with an additional bit of text.
withNonlocAtom2 :: (IsGameData (GameData g),
                    IsGameState (GameState g),
                    Monad m) =>
    ScriptMessage
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withNonlocAtom2 submsg msg [pdx| %_ = ?txt |] = do
    extratext <- messageText submsg
    msgToPP $ msg extratext txt
withNonlocAtom2 _ _ stmt = preStatement stmt

-- Table of script atom -> icon key. Only ones that are different are listed.
scriptIconTable :: HashMap Text Text
scriptIconTable = HM.fromList
    [
--  ,("democratic_utopia", "democratic utopia")
    ]

-- Given a script atom, return the corresponding icon key, if any.
iconKey :: Text -> Maybe Text
iconKey atom = HM.lookup atom scriptIconTable

-- Numeric statement.
-- TODO (if necessary): allow operators other than = and pass them to message
-- handler
numeric :: (IsGameState (GameState g), Monad m) =>
    (Double -> ScriptMessage)
        -> StatementHandler g m
numeric msg [pdx| %_ = !n |] = msgToPP $ msg n
numeric _ stmt = plainMsg $ pre_statement' stmt

-- Generic statement referring to a country. Use a flag.
withFlag :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withFlag msg [pdx| %_ = $who |] = msgToPP =<< do
    whoflag <- flag who
    return . msg . Doc.doc2text $ whoflag
withFlag _ stmt = plainMsg $ pre_statement' stmt

withBool :: (IsGameState (GameState g), Monad m) =>
    (Bool -> ScriptMessage)
        -> StatementHandler g m
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (preStatement stmt)
          return
          fullmsg

withBool' :: (IsGameState (GameState g), Monad m) =>
    (Bool -> ScriptMessage)
        -> GenericStatement
        -> PPT g m (Maybe IndentedMessages)
withBool' msg [pdx| %_ = ?yn |] | T.map toLower yn `elem` ["yes","no","false"]
    = fmap Just . msgToPP $ case T.toCaseFold yn of
        "yes" -> msg True
        "no"  -> msg False
        "false" -> msg False
        _     -> error "impossible: withBool matched a string that wasn't yes, no or false"
withBool' _ _ = return Nothing

-- Statement may have "yes"/"no" or a tag.
withFlagOrBool :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    (Bool -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
withFlagOrBool bmsg _ [pdx| %_ = yes |] = msgToPP (bmsg True)
withFlagOrBool bmsg _ [pdx| %_ = no  |]  = msgToPP (bmsg False)
withFlagOrBool _ tmsg stmt = withFlag tmsg stmt

numericIcon :: (IsGame g, IsGameState (GameState g), Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numericIcon the_icon msg [pdx| %_ = !amt |]
    = msgToPP $ msg (iconText the_icon) amt
numericIcon _ _ stmt = plainMsg $ pre_statement' stmt

numericIconBonus :: (IsGame g, IsGameState (GameState g), Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> (Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numericIconBonus the_icon plainmsg yearlymsg [pdx| %_ = !amt |]
    = do
        mscope <- getCurrentScope
        let icont = iconText the_icon
            yearly = msgToPP $ yearlymsg icont amt
        case mscope of
            Nothing -> yearly -- ideas / bonuses
            Just thescope -> case thescope of
                _ -> -- act as though it's country for all others
                    msgToPP $ plainmsg icont amt
numericIconBonus _ _ _ stmt = plainMsg $ pre_statement' stmt

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

-- | Synonym for 'getGameL10nIfPresent'.
tryLoc :: (IsGameData (GameData g), Monad m) => Text -> PPT g m (Maybe Text)
tryLoc = getGameL10nIfPresent

data TextValue = TextValue
        {   tv_what :: Maybe Text
        ,   tv_value :: Maybe Double
        }
newTV :: TextValue
newTV = TextValue Nothing Nothing
textValue :: forall g m. (IsGameState (GameState g), Monad m) =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textValue whatlabel vallabel smallmsg bigmsg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (foldl' addLine newTV scr)
    where
        addLine :: TextValue -> GenericStatement -> TextValue
        addLine tv [pdx| $label = ?what |] | label == whatlabel
            = tv { tv_what = Just what }
        addLine tv [pdx| $label = !val |] | label == vallabel
            = tv { tv_value = Just val }
        addLine nor _ = nor
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv) of
            (Just what, Just value) -> do
                mwhat_loc <- loc what
                let what_icon = iconText what
                    what_loc = fromMaybe ("<tt>" <> what <> "</tt>") mwhat_loc
                return $ (if abs value < 1 then smallmsg else bigmsg) what_icon what_loc value
            _ -> return $ preMessage stmt
textValue _ _ _ _ _ stmt = preStatement stmt

-- | Statements of the form
-- @
--      has_trade_modifier = {
--          who = ROOT
--          name = merchant_recalled
--      }
-- @
data TextAtom = TextAtom
        {   ta_what :: Maybe Text
        ,   ta_atom :: Maybe Text
        }
-- | Empty 'TextAtom'
newTA :: TextAtom
newTA = TextAtom Nothing Nothing
-- | Interpret a statement of this form.
textAtom :: forall g m. (IsGameData (GameData g),
                         IsGameState (GameState g),
                         Monad m) =>
    Text -- ^ Label for "what"
        -> Text -- ^ Label for atom
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textAtom whatlabel atomlabel msg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ta (foldl' addLine newTA scr)
    where
        addLine :: TextAtom -> GenericStatement -> TextAtom
        addLine ta [pdx| $label = ?what |]
            | label == whatlabel
            = ta { ta_what = Just what }
        addLine ta [pdx| $label = ?at |]
            | label == atomlabel
            = ta { ta_atom = Just at }
        addLine nor _ = nor
        pp_ta :: TextAtom -> PPT g m ScriptMessage
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_icon = iconText what
                    what_loc = fromMaybe ("<tt>" <> what <> "</tt>") mwhat_loc
                return $ msg what_icon what_loc atom_loc
            _ -> return $ preMessage stmt
textAtom _ _ _ _ stmt = preStatement stmt

{-
-- AI decision factors
-- Most of the code for this is in HOI4.SuperCommon and re-exported here,
-- because HOI4.IdeaGroups needs them. But only HOI4.Common needs output
-- functions.
ppAiWillDo :: Monad m => AIWillDo -> PPT g m IndentedMessages
ppAiWillDo (AIWillDo mbase mods) = do
    mods_pp'd <- fold <$> traverse ppAiMod mods
    let baseWtMsg = case mbase of
            Nothing -> MsgNoBaseWeight
            Just base -> MsgAIBaseWeight base
    iBaseWtMsg <- msgToPP baseWtMsg
    return $ iBaseWtMsg ++ mods_pp'd

ppAiMod :: Monad m => AIModifier -> PPT g m IndentedMessages
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
-}

---------------------------------
-- Specific statement handlers --
---------------------------------

-- | Modifiers
addModifier :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
     StatementHandler g m
addModifier stmt@(Statement _ OpEq (CompoundRhs
        [[pdx| modifier = ?mod_name |]
        ,[pdx| days     = !mod_days |]]))
    = msgToPP =<< do
        name_loc <- getGameL10n mod_name
        return $ MsgAddMod mod_name name_loc mod_days
addModifier stmt = preStatement stmt

-- Opinions

-- | Add an opinion modifier towards someone (for a number of years).
data AddOpinion = AddOpinion {
        op_who :: Maybe Text
    ,   op_modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show
-- | Empty AddOpinion
newAddOpinion :: AddOpinion
newAddOpinion = AddOpinion Nothing Nothing Nothing

-- | Handler for adding an opinion modifier.
opinion :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    (Text -> Text -> ScriptMessage)
        -> (Text -> Text -> Double -> ScriptMessage)
        -> StatementHandler g m
opinion msgIndef msgDur stmt@(Statement _ OpEq (CompoundRhs scr))
    = msgToPP =<< pp_add_opinion (foldl' addLine newAddOpinion scr)
    where
        addLine :: AddOpinion -> GenericStatement -> AddOpinion
        addLine op [pdx| who      = $tag   |] = op { op_who = Just tag }
        addLine op [pdx| modifier = ?label |] = op { op_modifier = Just label }
        addLine op [pdx| years    = !n     |] = op { op_years = Just n }
        addLine op _ = op
        pp_add_opinion op = case (op_who op, op_modifier op) of
            (Just whom, Just modifier) -> do
                mod_loc <- getGameL10n modifier
                case op_years op of
                    Nothing -> return $ msgIndef mod_loc whom
                    Just years -> return $ msgDur mod_loc whom years
            _ -> trace ("failed! modifier op is " ++ show (op_modifier op)) $ return (preMessage stmt)
opinion _ _ stmt = preStatement stmt

-- | Trigger for comparing opinion.
data HasOpinion = HasOpinion
        {   hop_who :: Maybe Text
        ,   hop_value :: Maybe Double
        }
-- | Empty 'HasOpinion'
newHasOpinion :: HasOpinion
newHasOpinion = HasOpinion Nothing Nothing
-- | Handler for comparing opinion.
hasOpinion :: forall g m. (IsGameState (GameState g), Monad m) =>
     StatementHandler g m
hasOpinion stmt@(Statement _ OpEq (CompoundRhs scr))
    = msgToPP =<< pp_hasOpinion (foldl' addLine newHasOpinion scr)
    where
        addLine :: HasOpinion -> GenericStatement -> HasOpinion
        addLine hop [pdx| who   = ?who |] = hop { hop_who = Just who }
        addLine hop [pdx| value = !val |] = hop { hop_value = Just val }
        addLine hop _ = trace "warning: unrecognized has_opinion clause" hop
        pp_hasOpinion :: HasOpinion -> PPT g m ScriptMessage
        pp_hasOpinion hop = case (hop_who hop, hop_value hop) of
            (Just who, Just value) ->
                return (MsgHasOpinion value who)
            _ -> return (preMessage stmt)
hasOpinion stmt = preStatement stmt

-- Events

-- | Triggering an event
data TriggerEvent = TriggerEvent
        { e_id :: Maybe Text
        , e_title_loc :: Maybe Text
        , e_days :: Maybe Double
        }
-- | Empty 'TriggerEvent'
newTriggerEvent :: TriggerEvent
newTriggerEvent = TriggerEvent Nothing Nothing Nothing
-- | Handler for event triggers.
triggerEvent :: forall g m. (HOI4Info g, Monad m) =>
    ScriptMessage
        -> StatementHandler g m
triggerEvent evtType stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_trigger_event =<< foldM addLine newTriggerEvent scr
    where
        addLine :: TriggerEvent -> GenericStatement -> PPT g m TriggerEvent
        addLine evt [pdx| id = $eid |] = do
            mevt_t <- getEventTitle eid
            t_loc <- fmap join (sequence (getGameL10nIfPresent <$> mevt_t))
            return evt { e_id = Just eid, e_title_loc = t_loc }
        addLine evt [pdx| days = %rhs |]
            = return evt { e_days = floatRhs rhs }
        addLine evt _ = return evt
        pp_trigger_event :: TriggerEvent -> PPT g m ScriptMessage
        pp_trigger_event evt = do
            evtType_t <- messageText evtType
            case e_id evt of
                Just msgid ->
                    let loc = fromMaybe msgid (e_title_loc evt)
                    in case e_days evt of
                        Just days -> return $ MsgTriggerEventDays evtType_t msgid loc days
                        Nothing -> return $ MsgTriggerEvent evtType_t msgid loc
                _ -> return $ preMessage stmt
triggerEvent _ stmt = preStatement stmt

-- Random

-- | Handler for @random@ blocks.
random :: (HOI4Info g, Monad m) =>
     StatementHandler g m
random stmt@[pdx| %_ = @scr |]
    | (front, back) <- break
                        (\substmt -> case substmt of
                            [pdx| chance = %_ |] -> True
                            _ -> False)
                        scr
      , not (null back)
      , [pdx| %_ = %rhs |] <- head back
      , Just chance <- floatRhs rhs
      = compoundMessage
          (MsgRandomChance chance)
          [pdx| %undefined = @(front ++ tail back) |]
    | otherwise = compoundMessage MsgRandom stmt
random stmt = preStatement stmt

-- | Handler for @random_list@ blocks.
randomList :: (HOI4Info g, Monad m) =>
     StatementHandler g m
randomList stmt@[pdx| %_ = @scr |] = fmtRandomList $ map entry scr
    where
        entry [pdx| !weight = @scr |] = (fromIntegral weight, scr)
        entry _ = error "Bad clause in random_list"
        fmtRandomList entries = withCurrentIndent $ \i ->
            let total = sum (map fst entries)
            in (:) <$> pure (i, MsgRandom)
                   <*> (concat <$> indentUp (mapM (fmtRandomList' total) entries))
        fmtRandomList' total (wt, what) = withCurrentIndent $ \i ->
            (:) <$> pure (i, MsgRandomChance ((wt / total) * 100))
                <*> ppMany what -- has integral indentUp
randomList stmt = withCurrentFile $ \file ->
    error ("error: randomList passed strange statement in " ++ file ++ ": " ++ show stmt)

-- DLC

-- | Handler for @has_dlc@.
hasDlc :: (IsGameState (GameState g), Monad m) => StatementHandler g m
hasDlc [pdx| %_ = ?dlc |]
    = msgToPP $ MsgHasDLC dlc_icon dlc
    where
        mdlc_key = HM.lookup dlc . HM.fromList $
            [ -- HOI4 has no major DLC yet.
            ]
        dlc_icon = maybe "" iconText mdlc_key
hasDlc stmt = preStatement stmt

-- Switch / Trigger switch

-- | A trigger switch must be of the form
-- @
--     trigger_switch = {
--         on_trigger = <statement lhs>
--         <statement rhs> = {
--             <actions>
--         }
--     }
-- @
-- where the <statement rhs> block may be repeated several times.
--
-- Switches are the same, but the head is `switch` and the trigger clause is
-- `trigger` instead of `on_trigger`. Semantically `trigger_switch` is used in
-- triggers; `switch` is used in actions.
triggerSwitch :: (HOI4Info g, Monad m) => StatementHandler g m
triggerSwitch stmt@(Statement _ OpEq (CompoundRhs
                    ([pdx| $triggerClause = $condlhs |] -- assume this is first statement
                    :clauses)))
                    | triggerClause `elem` ["trigger", "on_trigger"] = do
    statementsMsgs <- indentUp $ forM clauses $ \clause -> case clause of
        -- using next indent level, for each block <condrhs> = { ... }:
        [pdx| $condrhs = @action |] -> do
            -- construct a fake condition to pp
            let cond = [pdx| $condlhs = $condrhs |]
            ((_, guardMsg):_) <- ppOne cond -- XXX: match may fail (but shouldn't)
            guardText <- messageText guardMsg
            -- pp the rest of the block, at the next level
            statementMsgs <- indentUp (ppMany action)
            withCurrentIndent $ \i -> return $ (i, MsgTriggerSwitchClause guardText) : statementMsgs
        _ -> preStatement stmt
    withCurrentIndent $ \i -> return $ (i, MsgTriggerSwitch) : concat statementsMsgs
triggerSwitch stmt = preStatement stmt

-- | Handler for @is_month@.
isMonth :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
     StatementHandler g m
isMonth [pdx| %_ = !(num :: Int) |] | num >= 1, num <= 12
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

-- | Handler for @custom_trigger_tooltip@. This bundles a trigger, which is
-- hidden, with a custom tooltip.
customTriggerTooltip :: (HOI4Info g, Monad m) =>
     StatementHandler g m
customTriggerTooltip [pdx| %_ = @scr |]
    -- ignore the custom tooltip
    = let rest = flip filter scr $ \stmt -> case stmt of
            [pdx| tooltip = %_ |] -> False
            _ -> True
      in indentDown $ ppMany rest
customTriggerTooltip stmt = preStatement stmt
