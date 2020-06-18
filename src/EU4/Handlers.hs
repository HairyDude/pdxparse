module EU4.Handlers (
        preStatement
    ,   plainMsg
    ,   msgToPP
    ,   flagText
    ,   isTag
    ,   pp_mtth
    ,   compound
    ,   compoundMessage
    ,   compoundMessagePronoun 
    ,   compoundMessageTagged
    ,   allowPronoun
    ,   withLocAtom
    ,   withLocAtom2
    ,   withLocAtomAndIcon
    ,   withLocAtomIcon
    ,   withLocAtomIconEU4Scope
    ,   locAtomTagOrProvince
    ,   withProvince
    ,   withNonlocAtom
    ,   withNonlocAtom2
    ,   iconKey
    ,   iconFile
    ,   iconFileB
    ,   iconOrFlag
    ,   tagOrProvince
    ,   tagOrProvinceIcon
    ,   numeric
    ,   numericOrTag
    ,   numericOrTagIcon
    ,   numericIconChange 
    ,   withFlag 
    ,   withBool
    ,   withFlagOrBool
    ,   withTagOrNumber 
    ,   numericIcon
    ,   numericIconLoc
    ,   numericIconBonus
    ,   tryLoc
    ,   tryLocAndIcon
    ,   textValue
    ,   textAtom
    ,   ppAiWillDo
    ,   ppAiMod
    ,   factionInfluence
    ,   factionInPower
    ,   addModifier
    ,   addCore
    ,   opinion
    ,   hasOpinion
    ,   spawnRebels
    ,   hasSpawnedRebels
    ,   canSpawnRebels
    ,   triggerEvent
    ,   gainMen
    ,   addCB
    ,   random
    ,   randomList
    ,   defineAdvisor
    ,   defineRuler
    ,   buildToForcelimit
    ,   declareWarWithCB
    ,   hasDlc
    ,   hasEstateInfluenceModifier
    ,   estateInfluenceModifier
    ,   triggerSwitch
    ,   calcTrueIf
    ,   defineHeir
    ,   hreReformLevel
    ,   religionYears
    ,   govtRank
    ,   setGovtRank
    ,   numProvinces
    ,   withFlagOrProvince
    ,   withFlagOrProvinceEU4Scope 
    ,   tradeMod
    ,   isMonth
    ,   range
    ,   area
    ,   dominantCulture
    ,   customTriggerTooltip
    ,   piety
    ,   hasIdea
    ,   trust
    ,   governmentPower
    -- testing
    ,   isPronoun
    ,   flag
    ) where

import Data.Char (toUpper, toLower, isUpper)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
--import Data.Set (Set)
import qualified Data.Set as S
import Data.Trie (Trie)
import qualified Data.Trie as Tr

import qualified Text.PrettyPrint.Leijen.Text as PP

import Data.List (foldl', intersperse)
import Data.Maybe (isJust, fromMaybe)

import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Monad (foldM, mplus, forM, join, when)
import Data.Foldable (fold)
import Data.Monoid ((<>))

import Abstract -- everything
import Doc (Doc)
import qualified Doc -- everything
import Messages -- everything
import MessageTools (plural)
import QQ -- everything
import SettingsTypes ( PPT, IsGameData (..), GameData (..), IsGameState (..), GameState (..)
                     , indentUp, indentDown, withCurrentIndent, withCurrentIndentZero, alsoIndent, alsoIndent'
                     , getGameL10n, getGameL10nIfPresent, getGameL10nDefault, withCurrentFile
                     , unfoldM, unsnoc )
import Templates
import {-# SOURCE #-} EU4.Common (pp_script, ppMany, ppOne)
import EU4.Types -- everything

import Debug.Trace

-- | Pretty-print a script statement, wrap it in a @<pre>@ element, and emit a
-- generic message for it at the current indentation level. This is the
-- fallback in case we haven't implemented that particular statement or we
-- failed to understand it.
preStatement :: (IsGameState (GameState g), Monad m) =>
    GenericStatement -> PPT g m IndentedMessages
preStatement stmt = (:[]) <$> alsoIndent' (preMessage stmt)

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

-- | Create a generic message from a piece of text. The rendering function will
-- pass this through unaltered.
plainMsg :: (IsGameState (GameState g), Monad m) => Text -> PPT g m IndentedMessages
plainMsg msg = (:[]) <$> (alsoIndent' . MsgUnprocessed $ msg)

msgToPP :: (IsGameState (GameState g), Monad m) => ScriptMessage -> PPT g m IndentedMessages
msgToPP msg = (:[]) <$> alsoIndent' msg

-- Emit icon template.
icon :: Text -> Doc
icon what = template "icon" [HM.lookupDefault what what scriptIconTable, "28px"]
iconText :: Text -> Text
iconText = Doc.doc2text . icon

-- Argument may be a tag or a tagged variable. Emit a flag in the former case,
-- and localize in the latter case.
eflag :: (EU4Info g, Monad m) =>
            Maybe EU4Scope -> Either Text (Text, Text) -> PPT g m (Maybe Text)
eflag expectScope = \case
    Left name -> Just <$> flagText expectScope name
    Right (vartag, var) -> tagged vartag var

-- | Look up the message corresponding to a tagged atom.
--
-- For example, to localize @event_target:some_name@, call
-- @tagged "event_target" "some_name"@.
tagged :: (EU4Info g, Monad m) =>
    Text -> Text -> PPT g m (Maybe Text)
tagged vartag var = case flip Tr.lookup varTags . TE.encodeUtf8 $ vartag of
    Just msg -> Just <$> messageText (msg var)
    Nothing -> return Nothing

flagText :: (EU4Info g, Monad m) =>
    Maybe EU4Scope -> Text -> PPT g m Text
flagText expectScope = fmap Doc.doc2text . flag expectScope

-- Emit an appropriate phrase if the given text is a pronoun, otherwise use the
-- provided localization function.
allowPronoun :: (EU4Info g, Monad m) =>
    Maybe EU4Scope -> (Text -> PPT g m Doc) -> Text -> PPT g m Doc
allowPronoun expectedScope getLoc name =
    if isPronoun name
        then pronoun expectedScope name
        else getLoc name

-- | Emit flag template if the argument is a tag, or an appropriate phrase if
-- it's a pronoun.
flag :: (EU4Info g, Monad m) =>
    Maybe EU4Scope -> Text -> PPT g m Doc
flag expectscope = allowPronoun expectscope $ \name ->
                    template "flag" . (:[]) <$> getGameL10n name

getScopeForPronoun :: (EU4Info g, Monad m) =>
    Text -> PPT g m (Maybe EU4Scope)
getScopeForPronoun = helper . T.toLower where
    helper "this" = getCurrentScope
    helper "root" = getRootScope
    helper "prev" = getPrevScope
    helper "controller" = return (Just EU4Country)
    helper "emperor" = return (Just EU4Country)
    helper "capital" = return (Just EU4Province)
    helper _ = return Nothing

-- | Emit an appropriate phrase for a pronoun.
-- If a scope is passed, that is the type the current command expects. If they
-- don't match, it's a synecdoche; adjust the wording appropriately.
--
-- All handlers in this module that take an argument of type 'Maybe EU4Scope'
-- call this function. Use whichever scope corresponds to what you expect to
-- appear on the RHS. If it can be one of several (e.g. either a country or a
-- province), use EU4From. If it doesn't correspond to any scope, use Nothing.
pronoun :: (EU4Info g, Monad m) =>
    Maybe EU4Scope -> Text -> PPT g m Doc
pronoun expectedScope name = withCurrentFile $ \f -> case T.toLower name of
    "root" -> getRootScope >>= \case -- will need editing
        Just EU4Country
            | expectedScope `matchScope` EU4Country -> message MsgROOTCountry
            | otherwise                             -> message MsgROOTCountryAsOther
        Just EU4Province
            | expectedScope `matchScope` EU4Province -> message MsgROOTProvince
            | expectedScope `matchScope` EU4Country -> message MsgROOTProvinceOwner
            | otherwise                             -> message MsgROOTProvinceAsOther
        -- No synecdoche possible
        Just EU4TradeNode -> message MsgROOTTradeNode
        -- No synecdoche possible
        Just EU4Geographic -> message MsgROOTGeographic
        _ -> return "ROOT"
    "prev" -> --do
--      ss <- getScopeStack
--      traceM (f ++ ": pronoun PREV: scope stack is " ++ show ss)
        getPrevScope >>= \_scope -> case _scope of -- will need editing
            Just EU4Country
                | expectedScope `matchScope` EU4Country -> message MsgPREVCountry
                | otherwise                             -> message MsgPREVCountryAsOther
            Just EU4Province
                | expectedScope `matchScope` EU4Province -> message MsgPREVProvince
                | expectedScope `matchScope` EU4Country -> message MsgPREVProvinceOwner
                | otherwise                             -> message MsgPREVProvinceAsOther
            Just EU4TradeNode -> message MsgPREVTradeNode
            Just EU4Geographic -> message MsgPREVGeographic
            _ -> return "PREV"
    "this" -> getCurrentScope >>= \case -- will need editing
        Just EU4Country
            | expectedScope `matchScope` EU4Country -> message MsgTHISCountry
            | otherwise                             -> message MsgTHISCountryAsOther
        Just EU4Province
            | expectedScope `matchScope` EU4Province -> message MsgTHISProvince
            | expectedScope `matchScope` EU4Country -> message MsgTHISProvinceOwner
            | otherwise                             -> message MsgTHISProvinceAsOther
        Just EU4TradeNode -> message MsgTHISTradeNode
        Just EU4Geographic -> message MsgTHISGeographic
        _ -> return "PREV"
    "controller" -> message MsgController
    "emperor" -> message MsgEmperor
    "original_dynasty" -> message MsgOriginalDynasty
    "historic_dynasty" -> message MsgHistoricDynasty
    "capital" -> message MsgCapital
    -- Suggestions of a description for FROM are welcome.
    _ -> return $ Doc.strictText name -- something else; regurgitate untouched
    where
        Nothing `matchScope` _ = True
        Just expect `matchScope` actual
            | expect == actual = True
            | otherwise        = False

isTag :: Text -> Bool
isTag s = T.length s == 3 && T.all isUpper s

-- Tagged messages
varTags :: Trie (Text -> ScriptMessage)
varTags = Tr.fromList . map (first TE.encodeUtf8) $
    [("event_target", MsgEventTargetVar)
    ]

isPronoun :: Text -> Bool
isPronoun s = T.map toLower s `S.member` pronouns where
    pronouns = S.fromList
        ["root"
        ,"prev"
        ,"this"
        ,"owner"
        ,"controller"
        ,"emperor"
        ,"original_dynasty"
        ,"historic_dynasty"
        ,"capital"
        ]

-- Get the localization for a province ID, if available.
getProvLoc :: (IsGameData (GameData g), Monad m) =>
    Int -> PPT g m Text
getProvLoc n =
    let provid_t = T.pack (show n)
    in getGameL10nDefault provid_t ("PROV" <> provid_t)

-----------------------------------------------------------------
-- Script handlers that should be used directly, not via ppOne --
-----------------------------------------------------------------

-- | Data for @mean_time_to_happen@ clauses
data MTTH = MTTH
        {   mtth_years :: Maybe Int
        ,   mtth_months :: Maybe Int
        ,   mtth_days :: Maybe Int
        ,   mtth_modifiers :: [MTTHModifier]
        } deriving Show
-- | Data for @modifier@ clauses within @mean_time_to_happen@ clauses
data MTTHModifier = MTTHModifier
        {   mtthmod_factor :: Maybe Double
        ,   mtthmod_conditions :: GenericScript
        } deriving Show
-- | Empty MTTH
newMTTH :: MTTH
newMTTH = MTTH Nothing Nothing Nothing []
-- | Empty MTTH modifier
newMTTHMod :: MTTHModifier
newMTTHMod = MTTHModifier Nothing []

-- | Format a @mean_time_to_happen@ clause as wiki text.
pp_mtth :: (EU4Info g, Monad m) => GenericScript -> PPT g m Doc
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

-- | Generic handler for a simple compound statement. Usually you should use
-- 'compoundMessage' instead so the text can be localized.
compound :: (EU4Info g, Monad m) =>
    Text -- ^ Text to use as the block header, without the trailing colon
    -> StatementHandler g m
compound header [pdx| %_ = @scr |]
    = withCurrentIndent $ \_ -> do -- force indent level at least 1
        headerMsg <- plainMsg (header <> ":")
        scriptMsgs <- ppMany scr
        return $ headerMsg ++ scriptMsgs
compound _ stmt = preStatement stmt

-- | Generic handler for a simple compound statement.
compoundMessage :: (EU4Info g, Monad m) =>
    ScriptMessage -- ^ Message to use as the block header
    -> StatementHandler g m
compoundMessage header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        script_pp'd <- ppMany scr
        return ((i, header) : script_pp'd)
compoundMessage _ stmt = preStatement stmt

-- | Generic handler for a simple compound statement headed by a pronoun.
compoundMessagePronoun :: (EU4Info g, Monad m) => StatementHandler g m
compoundMessagePronoun stmt@[pdx| $head = @scr |] = withCurrentIndent $ \i -> do
    params <- withCurrentFile $ \f -> case T.toLower head of
        "root" -> do
                newscope <- getRootScope
                return (newscope, case newscope of
                    Just EU4Country -> Just MsgROOTCountry
                    Just EU4Province -> Just MsgROOTProvince
                    Just EU4TradeNode -> Just MsgROOTTradeNode
                    Just EU4Geographic -> Just MsgROOTGeographic
                    Just EU4Bonus ->
                        trace (f ++ ": compoundMessagePronoun called in bonus scope")
                        $ Nothing
                    Just EU4From ->
                        trace (f ++ ": compoundMessagePronoun for ROOT somehow had FROM scope")
                        $ Nothing
                    Nothing -> 
                        trace (f ++ ": compoundMessagePronoun for ROOT somehow had no scope")
                        $ Nothing)
        "prev" -> do
                newscope <- getPrevScope
                return (newscope, case newscope of
                    Just EU4Country -> Just MsgPREVCountry
                    Just EU4Province -> Just MsgPREVProvince
                    Just EU4TradeNode -> Just MsgPREVTradeNode
                    Just EU4Geographic -> Just MsgPREVGeographic
                    Just EU4Bonus ->
                        trace (f ++ ": compoundMessagePronoun called in bonus scope")
                        $ Nothing
                    Just EU4From -> Just MsgPREV -- Roll with it
                    Nothing -> 
                        trace (f ++ ": compoundMessagePronoun for PREV somehow had no scope")
                        $ Nothing)
        "from" -> return (Just EU4From, Just MsgFROM) -- don't know what type this is in general
        _ -> trace (f ++ ": compoundMessagePronoun: don't know how to handle head " ++ T.unpack head)
             $ return (Nothing, undefined)
    case params of
        (Nothing, _) -> preStatement stmt
        (_, Nothing) -> preStatement stmt
        (Just newscope, Just scopemsg) -> do
            script_pp'd <- scope newscope $ ppMany scr
            return $ (i, scopemsg) : script_pp'd
compoundMessagePronoun stmt = preStatement stmt

-- | Generic handler for a simple compound statement with a tagged header.
compoundMessageTagged :: (EU4Info g, Monad m) =>
    (Text -> ScriptMessage) -- ^ Message to use as the block header
    -> Maybe EU4Scope -- ^ Scope to push on the stack, if any
    -> StatementHandler g m
compoundMessageTagged header mscope stmt@[pdx| $_:$tag = %_ |]
    = (case mscope of
        Just newscope -> scope newscope
        Nothing -> id) $ compoundMessage (header tag) stmt
compoundMessageTagged _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom.
withLocAtom :: (IsGameData (GameData g),
                IsGameState (GameState g),
                Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withLocAtom msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> getGameL10n key
withLocAtom _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom and we
-- need a second one (passed to message as first arg).
withLocAtom2 :: (IsGameData (GameData g),
                 IsGameState (GameState g),
                 Monad m) =>
    ScriptMessage
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtom2 inMsg msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> pure key <*> messageText inMsg <*> getGameL10n key
withLocAtom2 _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom, where we
-- also need an icon.
withLocAtomAndIcon :: (EU4Info g, Monad m) =>
    Text -- ^ icon name - see
         -- <https://www.eu4wiki.com/Template:Icon Template:Icon> on the wiki
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomAndIcon iconkey msg [pdx| %_ = $key |]
    = do what <- Doc.doc2text <$> allowPronoun Nothing (fmap Doc.strictText . getGameL10n) key
         msgToPP $ msg (iconText iconkey) what
withLocAtomAndIcon _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom that
-- corresponds to an icon.
withLocAtomIcon :: (EU4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomIcon msg stmt@[pdx| %_ = $key |]
    = withLocAtomAndIcon key msg stmt
withLocAtomIcon _ stmt = preStatement stmt

-- | Generic handler for a statement that needs both an atom and an icon, whose
-- meaning changes depending on which scope it's in.
withLocAtomIconEU4Scope :: (EU4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -- ^ Message for country scope
        -> (Text -> Text -> ScriptMessage) -- ^ Message for province scope
        -> StatementHandler g m
withLocAtomIconEU4Scope countrymsg provincemsg stmt = do
    thescope <- getCurrentScope
    case thescope of
        Just EU4Country -> withLocAtomIcon countrymsg stmt
        Just EU4Province -> withLocAtomIcon provincemsg stmt
        _ -> preStatement stmt -- others don't make sense

-- | Generic handler for a statement where the RHS is a localizable atom, but
-- may be replaced with a tag or province to refer synecdochally to the
-- corresponding value.
locAtomTagOrProvince :: (EU4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage) -- ^ Message for atom
        -> (Text -> ScriptMessage) -- ^ Message for synecdoche
        -> StatementHandler g m
locAtomTagOrProvince atomMsg synMsg stmt@[pdx| %_ = $val |] =
    if isTag val || isPronoun val
       then tagOrProvinceIcon synMsg synMsg stmt
       else withLocAtomIcon atomMsg stmt
locAtomTagOrProvince _ _ stmt = preStatement stmt

withProvince :: (EU4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withProvince msg stmt@[pdx| %lhs = $vartag:$var |] = do
    mtagloc <- tagged vartag var
    case mtagloc of
        Just tagloc -> msgToPP $ msg tagloc
        Nothing -> preStatement stmt
withProvince msg stmt@[pdx| %lhs = $var |]
    = msgToPP =<< msg . Doc.doc2text <$> pronoun (Just EU4Province) var
withProvince msg [pdx| %lhs = !provid |]
    = withLocAtom msg [pdx| %lhs = $(T.pack ("PROV" <> show (provid::Int))) |]
withProvince _ stmt = preStatement stmt

-- As withLocAtom but no l10n.
withNonlocAtom :: (IsGameState (GameState g), Monad m) => (Text -> ScriptMessage) -> StatementHandler g m
withNonlocAtom msg [pdx| %_ = ?text |] = msgToPP $ msg text
withNonlocAtom _ stmt = preStatement stmt

-- | As withlocAtom but wth no l10n and an additional bit of text.
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

-- | Table of script atom -> icon key. Only ones that are different are listed.
scriptIconTable :: HashMap Text Text
scriptIconTable = HM.fromList
    [("administrative_ideas", "administrative")
    ,("age_of_absolutism", "age of absolutism")
    ,("age_of_discovery", "age of discovery")
    ,("age_of_reformation", "age of reformation")
    ,("age_of_revolutions", "age of revolutions")
    ,("aristocracy_ideas", "aristocratic")
    ,("army_organizer", "army organizer")
    ,("army_organiser", "army organizer") -- both are used
    ,("army_reformer", "army reformer")
    ,("base_production", "production")
    ,("colonial_governor", "colonial governor")
    ,("diplomat", "diplomat_adv")
    ,("diplomatic_ideas", "diplomatic")
    ,("economic_ideas", "economic")
    ,("estate_burghers", "burghers")
    ,("estate_church", "clergy")
    ,("estate_cossacks", "cossacks")
    ,("estate_dhimmi", "dhimmi")
    ,("estate_nobles", "nobles")
    ,("estate_nomadic_tribes", "tribes")
    ,("grand_captain", "grand captain")
    ,("influence_ideas", "influence")
    ,("innovativeness_ideas", "innovative")
    ,("is_monarch_leader", "ruler general")
    ,("master_of_mint", "master of mint")
    ,("master_recruiter", "master recruiter")
    ,("mesoamerican_religion", "mayan")
    ,("military_engineer", "military engineer")
    ,("natural_scientist", "natural scientist")
    ,("naval_reformer", "naval reformer")
    ,("navy_reformer", "naval reformer") -- these are both used!
    ,("nomad_group", "nomadic")
    ,("norse_pagan_reformed", "norse")
    ,("particularist", "particularists")
    ,("piety", "being pious") -- chosen arbitrarily
    ,("religious_ideas", "religious")
    ,("shamanism", "fetishism") -- religion reused
    ,("spy_ideas", "espionage")
    ,("tengri_pagan_reformed", "tengri")
    ,("trade_ideas", "trade")
    -- religious schools
    ,("hanafi_school", "hanafi")
    ,("hanbali_school", "hanbali")
    ,("maliki_school", "maliki")
    ,("shafii_school", "shafii")
    ,("ismaili_school", "ismaili")
    ,("jafari_school", "jafari")
    ,("zaidi_school", "zaidi")
    ]

-- Given a script atom, return the corresponding icon key, if any.
iconKey :: Text -> Maybe Text
iconKey atom = HM.lookup atom scriptIconTable

-- | Table of icon tag to wiki filename. Only those that are different are
-- listed.
iconFileTable :: HashMap Text Text
iconFileTable = HM.fromList
    [("global tax modifier", "national tax modifier")
    ,("stability cost", "stability cost modifier")
    ,("land maintenance", "land maintenance modifier")
    ,("tolerance of the true faith", "tolerance own")
    ,("light ship combat ability", "light ship power")
    ]

-- | Given an {{icon}} key, give the corresponding icon file name.
--
-- Needed for idea groups, which don't use {{icon}}.
iconFile :: Text -> Text
iconFile s = HM.lookupDefault s s iconFileTable
-- | ByteString version of 'iconFile'.
iconFileB :: ByteString -> ByteString
iconFileB = TE.encodeUtf8 . iconFile . TE.decodeUtf8

-- | As generic_icon except
--
-- * say "same as <foo>" if foo refers to a country (in which case, add a flag if possible)
-- * may not actually have an icon (localization file will know if it doesn't)
iconOrFlag :: (EU4Info g, Monad m) =>
    (Text -> Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> Maybe EU4Scope
        -> StatementHandler g m
iconOrFlag _ flagmsg expectScope stmt@[pdx| %_ = $vartag:$var |] = do
    mwhoflag <- eflag expectScope (Right (vartag, var))
    case mwhoflag of
        Just whoflag -> msgToPP . flagmsg $ whoflag
        Nothing -> preStatement stmt
iconOrFlag iconmsg flagmsg expectScope [pdx| $head = $name |] = msgToPP =<< do
    nflag <- flag expectScope name -- laziness means this might not get evaluated
--   when (T.toLower name == "prev") . withCurrentFile $ \f -> do
--       traceM $ f ++ ": iconOrFlag: " ++ T.unpack head ++ " = " ++ T.unpack name
--       ps <- getPrevScope
--       traceM $ "PREV scope is: " ++ show ps
    if isTag name || isPronoun name
        then return . flagmsg . Doc.doc2text $ nflag
        else iconmsg <$> return (iconText . HM.lookupDefault name name $ scriptIconTable)
                     <*> getGameL10n name
iconOrFlag _ _ _ stmt = plainMsg $ pre_statement' stmt

-- | Message with icon and tag.
withFlagAndIcon :: (EU4Info g, Monad m) =>
    Text
        -> (Text -> Text -> ScriptMessage)
        -> Maybe EU4Scope
        -> StatementHandler g m
withFlagAndIcon iconkey flagmsg expectScope stmt@[pdx| %_ = $vartag:$var |] = do
    mwhoflag <- eflag expectScope (Right (vartag, var))
    case mwhoflag of
        Just whoflag -> msgToPP . flagmsg (iconText iconkey) $ whoflag
        Nothing -> preStatement stmt
withFlagAndIcon iconkey flagmsg expectScope [pdx| %_ = $name |] = msgToPP =<< do
    nflag <- flag expectScope name
    return . flagmsg (iconText iconkey) . Doc.doc2text $ nflag
withFlagAndIcon _ _ _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for statements where RHS is a tag or province id.
tagOrProvince :: (EU4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> Maybe EU4Scope
        -> StatementHandler g m
tagOrProvince tagmsg provmsg expectScope stmt@[pdx| %_ = ?!eobject |]
    = msgToPP =<< case eobject of
            Just (Right tag) -> do
                tagflag <- flag expectScope tag
                return . tagmsg . Doc.doc2text $ tagflag
            Just (Left provid) -> do -- is a province id
                prov_loc <- getProvLoc provid
                return . provmsg $ prov_loc
            Nothing -> return (preMessage stmt)
tagOrProvince _ _ _ stmt = preStatement stmt

tagOrProvinceIcon :: (EU4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
tagOrProvinceIcon tagmsg provmsg stmt@[pdx| $head = ?!eobject |]
    = msgToPP =<< case eobject of
            Just (Right tag) -> do -- string: is a tag or pronoun
--              when (T.toLower tag == "prev") . withCurrentFile $ \f -> do
--                  traceM $ f ++ ": tagOrProvinceIcon: " ++ T.unpack head ++ " = " ++ T.unpack tag
--                  ps <- getPrevScope
--                  traceM $ "PREV scope is: " ++ show ps
                tagflag <- flag Nothing tag
                return . tagmsg . Doc.doc2text $ tagflag
            Just (Left provid) -> do -- is a province id
                prov_loc <- getProvLoc provid
                return . provmsg $ prov_loc
            Nothing -> return (preMessage stmt)
tagOrProvinceIcon _ _ stmt = preStatement stmt

-- TODO (if necessary): allow operators other than = and pass them to message
-- handler
-- | Handler for numeric statements.
numeric :: (IsGameState (GameState g), Monad m) =>
    (Double -> ScriptMessage)
        -> StatementHandler g m
numeric msg [pdx| %_ = !n |] = msgToPP $ msg n
numeric _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for statements where the RHS is either a number or a tag.
numericOrTag :: (EU4Info g, Monad m) =>
    (Double -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
numericOrTag numMsg tagMsg stmt@[pdx| %_ = %rhs |] = msgToPP =<<
    case floatRhs rhs of
        Just n -> return $ numMsg n
        Nothing -> case textRhs rhs of
            Just t -> do -- assume it's a country
                tflag <- flag (Just EU4Country) t
                return $ tagMsg (Doc.doc2text tflag)
            Nothing -> return (preMessage stmt)
numericOrTag _ _ stmt = preStatement stmt

-- | Handler for statements where the RHS is either a number or a tag, that
-- also require an icon.
numericOrTagIcon :: (EU4Info g, Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
numericOrTagIcon icon numMsg tagMsg stmt@[pdx| %_ = %rhs |] = msgToPP =<<
    case floatRhs rhs of
        Just n -> return $ numMsg icon n
        Nothing -> case textRhs rhs of
            Just t -> do -- assume it's a country
                tflag <- flag (Just EU4Country) t
                return $ tagMsg (iconText icon) (Doc.doc2text tflag)
            Nothing -> return (preMessage stmt)
numericOrTagIcon _ _ _ stmt = preStatement stmt

-- | Handler for a statement referring to a country. Use a flag.
withFlag :: (EU4Info g, Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withFlag msg stmt@[pdx| %_ = $vartag:$var |] = do
    mwhoflag <- eflag (Just EU4Country) (Right (vartag, var))
    case mwhoflag of
        Just whoflag -> msgToPP . msg $ whoflag
        Nothing -> preStatement stmt
withFlag msg [pdx| %_ = $who |] = do
    whoflag <- flag (Just EU4Country) who
    msgToPP . msg . Doc.doc2text $ whoflag
withFlag _ stmt = preStatement stmt

-- | Handler for yes-or-no statements.
withBool :: (IsGameState (GameState g), Monad m) =>
    (Bool -> ScriptMessage)
        -> StatementHandler g m
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (preStatement stmt)
          return
          fullmsg

-- | Helper for 'withBool'.
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

-- | Handler for statements whose RHS may be "yes"/"no" or a tag.
withFlagOrBool :: (EU4Info g, Monad m) =>
    (Bool -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
withFlagOrBool bmsg _ [pdx| %_ = yes |] = msgToPP (bmsg True)
withFlagOrBool bmsg _ [pdx| %_ = no  |]  = msgToPP (bmsg False)
withFlagOrBool _ tmsg stmt = withFlag tmsg stmt

-- | Handler for statements whose RHS is a number OR a tag/prounoun, with icon
withTagOrNumber :: (EU4Info g, Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withTagOrNumber iconkey numMsg _ scr@[pdx| %_ = %num |]
    | FloatRhs _ <- num = numericIcon iconkey numMsg scr
withTagOrNumber iconkey _ tagMsg scr@[pdx| %_ = $_ |]
    = withFlagAndIcon iconkey tagMsg (Just EU4Country) scr
withTagOrNumber  _ _ _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for statements that have a number and an icon.
numericIcon :: (IsGameState (GameState g), Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numericIcon the_icon msg [pdx| %_ = !amt |]
    = msgToPP $ msg (iconText the_icon) amt
numericIcon _ _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for statements that have a number and an icon, plus a fixed
-- localizable atom.
numericIconLoc :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    Text
        -> Text
        -> (Text -> Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numericIconLoc the_icon what msg [pdx| %_ = !amt |]
    = do whatloc <- getGameL10n what
         msgToPP $ msg (iconText the_icon) whatloc amt
numericIconLoc _ _ _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for statements that have a number and an icon, whose meaning
-- differs depending on what scope it's in.
numericIconBonus :: (EU4Info g, Monad m) =>
    Text
        -> (Text -> Double -> ScriptMessage) -- ^ Message for bonus scope
        -> (Text -> Double -> ScriptMessage) -- ^ Message for country / other scope
        -> StatementHandler g m
numericIconBonus the_icon plainmsg yearlymsg [pdx| %_ = !amt |]
    = do
        mscope <- getCurrentScope
        let icont = iconText the_icon
            yearly = msgToPP $ yearlymsg icont amt
        case mscope of
            Nothing -> yearly -- ideas / bonuses
            Just thescope -> case thescope of
                EU4Bonus -> yearly
                _ -> -- act as though it's country for all others
                    msgToPP $ plainmsg icont amt
numericIconBonus _ _ _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for values that use a different message and icon depending on
-- whether the value is positive or negative.
numericIconChange :: (EU4Info g, Monad m) =>
    Text        -- ^ Icon for negative values
        -> Text -- ^ Icon for positive values
        -> (Text -> Double -> ScriptMessage) -- ^ Message for negative values
        -> (Text -> Double -> ScriptMessage) -- ^ Message for positive values
        -> StatementHandler g m
numericIconChange negicon posicon negmsg posmsg [pdx| %_ = !amt |]
    = if amt < 0
        then msgToPP $ negmsg (iconText negicon) amt
        else msgToPP $ posmsg (iconText posicon) amt
numericIconChange _ _ _ _ stmt = plainMsg $ pre_statement' stmt

----------------------
-- Text/value pairs --
----------------------

-- $textvalue
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
-- There are several statements of this form, but with different "what" and
-- "value" labels, so the first two parameters say what those label are.
--
-- There are two message parameters, one for value < 1 and one for value >= 1.
-- In the example num_of_religion, value is interpreted as a percentage of
-- provinces if less than 1, or a number of provinces otherwise. These require
-- rather different messages.
--
-- We additionally attempt to localize the RHS of "what". If it has no
-- localization string, it gets wrapped in a @<tt>@ element instead.

-- convenience synonym
tryLoc :: (IsGameData (GameData g), Monad m) => Text -> PPT g m (Maybe Text)
tryLoc = getGameL10nIfPresent

-- | Get icon and localization for the atom given. Return @mempty@ if there is
-- no icon, and wrapped in @<tt>@ tags if there is no localization.
tryLocAndIcon :: (IsGameData (GameData g), Monad m) => Text -> PPT g m (Text,Text)
tryLocAndIcon atom = do
    loc <- tryLoc atom
    return (maybe mempty id (Just (iconText atom)),
            maybe ("<tt>" <> atom <> "</tt>") id loc)

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
        -> (Text -> PPT g m (Text, Text)) -- ^ Action to localize and get icon (applied to RHS of "what")
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
                (what_icon, what_loc) <- loc what
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
newTA :: TextAtom
newTA = TextAtom Nothing Nothing
textAtom :: forall g m. (IsGameData (GameData g),
                         IsGameState (GameState g),
                         Monad m) =>
    Text -- ^ Label for "what" (e.g. "who")
        -> Text -- ^ Label for atom (e.g. "name")
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

-- AI decision factors

-- | Extract the appropriate message(s) from an @ai_will_do@ clause.
ppAiWillDo :: (EU4Info g, Monad m) => AIWillDo -> PPT g m IndentedMessages
ppAiWillDo (AIWillDo mbase mods) = do
    mods_pp'd <- fold <$> traverse ppAiMod mods
    let baseWtMsg = case mbase of
            Nothing -> MsgNoBaseWeight
            Just base -> MsgAIBaseWeight base
    iBaseWtMsg <- msgToPP baseWtMsg
    return $ iBaseWtMsg ++ mods_pp'd

-- | Extract the appropriate message(s) from a @modifier@ section within an
-- @ai_will_do@ clause.
ppAiMod :: (EU4Info g, Monad m) => AIModifier -> PPT g m IndentedMessages
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

-- | Convert the atom used in scripts for a faction to the corresponding icon
-- key for its influence.
facInfluence_iconkey :: Text -> Maybe Text
facInfluence_iconkey fac = case fac of
        -- Celestial empire
        "enuchs" {- sic -} -> Just "eunuchs influence"
        "temples"          -> Just "temples influence"
        "bureaucrats"      -> Just "bureaucrats influence"
        -- Merchant republic
        "mr_aristocrats"   -> Just "aristocrats influence"
        "mr_guilds"        -> Just "guilds influence"
        "mr_traders"       -> Just "traders influence"
        -- Revolutionary republic
        "rr_jacobins"      -> Just "jacobin influence"
        "rr_royalists"     -> Just "royalist influence"
        "rr_girondists"    -> Just "girondist influence"
        _ {- unknown -}    -> Nothing

-- | Convert the atom used in scripts for a faction to the corresponding icon
-- key.
fac_iconkey :: Text -> Maybe Text
fac_iconkey fac = case fac of
        -- Celestial empire
        "enuchs" {- sic -} -> Just "eunuchs"
        "temples"          -> Just "temples"
        "bureaucrats"      -> Just "bureaucrats"
        -- Merchant republic
        "mr_aristocrats"   -> Just "aristocrats"
        "mr_guilds"        -> Just "guilds"
        "mr_traders"       -> Just "traders"
        -- Revolutionary republic
        "rr_jacobins"      -> Just "jacobins"
        "rr_royalists"     -> Just "royalists"
        "rr_girondists"    -> Just "girondists"
        _ {- unknown -}    -> Nothing

data FactionInfluence = FactionInfluence {
        faction :: Maybe Text
    ,   influence :: Maybe Double
    }
-- | Empty 'FactionInfluence'
newInfluence :: FactionInfluence
newInfluence = FactionInfluence Nothing Nothing
-- | Handler for adding faction influence.
factionInfluence :: (IsGameData (GameData g),
                     IsGameState (GameState g),
                     Monad m) => StatementHandler g m
factionInfluence stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_influence (foldl' addField newInfluence scr)
    where
        pp_influence inf = case (faction inf, influence inf) of
            (Just fac, Just infl) ->
                let fac_icon = maybe ("<!-- " <> fac <> " -->") iconText (facInfluence_iconkey fac)
                in do
                    fac_loc <- getGameL10n fac
                    return $ MsgFactionGainInfluence fac_icon fac_loc infl
            _ -> return $ preMessage stmt
        addField :: FactionInfluence -> GenericStatement -> FactionInfluence
        addField inf [pdx| faction   = ?fac |] = inf { faction = Just fac }
        addField inf [pdx| influence = !amt |] = inf { influence = Just amt }
        addField inf _ = inf -- unknown statement
factionInfluence stmt = preStatement stmt

-- | Handler for trigger checking which faction is in power.
factionInPower :: (IsGameData (GameData g),
                   IsGameState (GameState g),
                   Monad m) => StatementHandler g m
factionInPower [pdx| %_ = ?fac |] | Just facKey <- fac_iconkey fac
    = do fac_loc <- getGameL10n fac
         msgToPP $ MsgFactionInPower (iconText facKey) fac_loc
factionInPower stmt = preStatement stmt

-- Modifiers

data AddModifier = AddModifier {
        amod_name :: Maybe Text
    ,   amod_key :: Maybe Text
    ,   amod_who :: Maybe Text
    ,   amod_duration :: Maybe Double
    ,   amod_power :: Maybe Double
    } deriving Show
newAddModifier :: AddModifier
newAddModifier = AddModifier Nothing Nothing Nothing Nothing Nothing

addModifierLine :: AddModifier -> GenericStatement -> AddModifier
addModifierLine apm [pdx| name     = ?name     |] = apm { amod_name = Just name }
addModifierLine apm [pdx| key      = ?key      |] = apm { amod_key = Just key }
addModifierLine apm [pdx| who      = ?tag      |] = apm { amod_who = Just tag }
addModifierLine apm [pdx| duration = !duration |] = apm { amod_duration = Just duration }
addModifierLine apm [pdx| power    = !power    |] = apm { amod_power = Just power }
addModifierLine apm _ = apm -- e.g. hidden = yes

maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f = maybe (return Nothing) (fmap Just . f)

addModifier :: (EU4Info g, Monad m) =>
    ScriptMessage -> StatementHandler g m
addModifier kind stmt@(Statement _ OpEq (CompoundRhs scr)) =
    let amod = foldl' addModifierLine newAddModifier scr
    in if isJust (amod_name amod) || isJust (amod_key amod) then do
        let mkey = amod_key amod
            mname = amod_name amod
        mthemod <- join <$> sequence (getModifier <$> mname) -- Nothing if trade modifier
        tkind <- messageText kind
        mwho <- maybe (return Nothing)
                      (fmap (Just . Doc.doc2text) . flag (Just EU4Country))
                      (amod_who amod)
        mname_loc <- maybeM getGameL10n mname
        mkey_loc <- maybeM getGameL10n mkey
        let mdur = amod_duration amod
            mname_or_key = maybe mkey Just mname
            mname_or_key_loc = maybe mkey_loc Just mname_loc
            meffect = modEffects <$> mthemod
        mpp_meffect <- scope EU4Bonus $ maybeM ppMany meffect

        case mname_or_key of
            Just modid ->
                -- default presented name to mod id
                let name_loc = fromMaybe modid mname_or_key_loc
                in case (mwho, amod_power amod, mdur, mpp_meffect) of
                    -- Event modifiers - expect effects
                    (Nothing,  Nothing,  Nothing, Just pp_effect)  -> do
                        msghead <- alsoIndent' (MsgGainMod modid tkind name_loc)
                        return (msghead : pp_effect)
                    (Nothing,  Nothing,  Just dur, Just pp_effect) -> do
                        msghead <- alsoIndent' (MsgGainModDur modid tkind name_loc dur)
                        return (msghead : pp_effect)
                    (Just who, Nothing,  Nothing, Just pp_effect)  -> do
                        msghead <- alsoIndent' (MsgActorGainsMod modid who tkind name_loc)
                        return (msghead : pp_effect)
                    (Just who, Nothing,  Just dur, Just pp_effect) -> do
                        msghead <- alsoIndent' (MsgActorGainsModDur modid who tkind name_loc dur)
                        return (msghead : pp_effect)
                    -- Trade power modifiers - expect no effects
                    (Nothing,  Just pow, Nothing, _)  -> msgToPP $ MsgGainModPow modid tkind name_loc pow
                    (Nothing,  Just pow, Just dur, _) -> msgToPP $ MsgGainModPowDur modid tkind name_loc pow dur
                    (Just who, Just pow, Nothing, _)  -> msgToPP $ MsgActorGainsModPow modid who tkind name_loc pow
                    (Just who, Just pow, Just dur, _) -> msgToPP $ MsgActorGainsModPowDur modid who tkind name_loc pow dur
                    _ -> do
                        traceM $ "strange modifier spec" ++ case (mkey, mname) of
                            (Just key, _) -> ": " ++ T.unpack key
                            (_, Just name) -> ": " ++ T.unpack name
                            _ -> ""
                        preStatement stmt
            _ -> preStatement stmt -- Must have mod id
    else preStatement stmt
addModifier _ stmt = preStatement stmt

-- Add core

-- "add_core = <n>" in country scope means "Gain core on <localize PROVn>"
-- "add_core = <tag>" in province scope means "<localize tag> gains core"
addCore :: (EU4Info g, Monad m) =>
    StatementHandler g m
addCore [pdx| %_ = $tag |]
  = msgToPP =<< do -- tag
    tagflag <- flagText (Just EU4Country) tag
    return $ MsgTagGainsCore tagflag
addCore [pdx| %_ = !num |]
  = msgToPP =<< do -- province
    prov <- getProvLoc num
    return $ MsgGainCoreOnProvince prov
addCore stmt = preStatement stmt

-- Opinions

-- Add an opinion modifier towards someone (for a number of years).
data AddOpinion = AddOpinion {
        op_who :: Maybe (Either Text (Text, Text))
    ,   op_modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show
newAddOpinion :: AddOpinion
newAddOpinion = AddOpinion Nothing Nothing Nothing

opinion :: (EU4Info g, Monad m) =>
    (Text -> Text -> Text -> ScriptMessage)
        -> (Text -> Text -> Text -> Double -> ScriptMessage)
        -> StatementHandler g m
opinion msgIndef msgDur stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_add_opinion (foldl' addLine newAddOpinion scr)
    where
        addLine :: AddOpinion -> GenericStatement -> AddOpinion
        addLine op [pdx| who           = $tag         |] = op { op_who = Just (Left tag) }
        addLine op [pdx| who           = $vartag:$var |] = op { op_who = Just (Right (vartag, var)) }
        addLine op [pdx| modifier      = ?label       |] = op { op_modifier = Just label }
        addLine op [pdx| years         = !n           |] = op { op_years = Just n }
        -- following two for add_mutual_opinion_modifier_effect
        addLine op [pdx| scope_country = $tag         |] = op { op_who = Just (Left tag) }
        addLine op [pdx| scope_country = $vartag:$var |] = op { op_who = Just (Right (vartag, var)) }
        addLine op [pdx| opinion_modifier = ?label    |] = op { op_modifier = Just label }
        addLine op _ = op
        pp_add_opinion op = case (op_who op, op_modifier op) of
            (Just ewhom, Just modifier) -> do
                mwhomflag <- eflag (Just EU4Country) ewhom
                mod_loc <- getGameL10n modifier
                case (mwhomflag, op_years op) of
                    (Just whomflag, Nothing) -> return $ msgIndef modifier mod_loc whomflag
                    (Just whomflag, Just years) -> return $ msgDur modifier mod_loc whomflag years
                    _ -> return (preMessage stmt)
            _ -> trace ("opinion: who or modifier missing: " ++ show stmt) $ return (preMessage stmt)
opinion _ _ stmt = preStatement stmt

data HasOpinion = HasOpinion
        {   hop_who :: Maybe Text
        ,   hop_value :: Maybe Double
        }
newHasOpinion :: HasOpinion
newHasOpinion = HasOpinion Nothing Nothing
hasOpinion :: forall g m. (EU4Info g, Monad m) =>
    StatementHandler g m
hasOpinion stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_hasOpinion (foldl' addLine newHasOpinion scr)
    where
        addLine :: HasOpinion -> GenericStatement -> HasOpinion
        addLine hop [pdx| who   = ?who |] = hop { hop_who = Just who }
        addLine hop [pdx| value = !val |] = hop { hop_value = Just val }
        addLine hop _ = trace "warning: unrecognized has_opinion clause" hop
        pp_hasOpinion :: HasOpinion -> PPT g m ScriptMessage
        pp_hasOpinion hop = case (hop_who hop, hop_value hop) of
            (Just who, Just value) -> do
                who_flag <- flag (Just EU4Country) who
                return (MsgHasOpinion value (Doc.doc2text who_flag))
            _ -> return (preMessage stmt)
hasOpinion stmt = preStatement stmt

-- Rebels

-- Render a rebel type atom (e.g. anti_tax_rebels) as their name and icon key.
-- This is needed because all religious rebels localize as simply "Religious" -
-- we want to be more specific.
rebel_loc :: HashMap Text (Text,Text)
rebel_loc = HM.fromList
        [("polish_noble_rebels",    ("Magnates", "magnates"))
        ,("lollard_rebels",         ("Lollard heretics", "lollards"))
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
        ,("anti_tax_rebels",        ("Peasants", "peasants"))
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
        ,("nationalist_rebels",     ("Separatist rebels", "separatists"))
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
newSpawnRebels :: SpawnRebels
newSpawnRebels = SpawnRebels Nothing Nothing Nothing False Nothing Nothing

spawnRebels :: forall g m. (EU4Info g, Monad m) =>
    Maybe Text -> StatementHandler g m
spawnRebels mtype stmt = msgToPP =<< spawnRebels' mtype stmt where
    spawnRebels' Nothing [pdx| %_ = @scr |]
        = pp_spawnRebels $ foldl' addLine newSpawnRebels scr
    spawnRebels' rtype [pdx| %_ = !size |]
        = pp_spawnRebels $ newSpawnRebels { rebelType = rtype, rebelSize = Just size }
    spawnRebels' _ stmt' = return (preMessage stmt')

    addLine :: SpawnRebels -> GenericStatement -> SpawnRebels
    addLine op [pdx| type   = $tag  |] = op { rebelType = Just tag }
    addLine op [pdx| size   = !n    |] = op { rebelSize = Just n }
    addLine op [pdx| friend = $tag  |] = op { friend = Just tag }
    addLine op [pdx| win    = yes   |] = op { win = True }
    addLine op [pdx| unrest = !n    |] = op { sr_unrest = Just n }
    addLine op [pdx| leader = ?name |] = op { sr_leader = Just name }
    addLine op _ = op

    pp_spawnRebels :: SpawnRebels -> PPT g m ScriptMessage
    pp_spawnRebels reb
        = case rebelSize reb of
            Just size -> do
                let rtype_loc_icon = flip HM.lookup rebel_loc =<< rebelType reb
                friendText <- case friend reb of
                    Just thefriend -> do
                        cflag <- flagText (Just EU4Country) thefriend
                        mtext <- messageText (MsgRebelsFriendlyTo cflag)
                        return (" (" <> mtext <> ")")
                    Nothing -> return ""
                leaderText <- case sr_leader reb of
                    Just leader -> do
                        mtext <- messageText (MsgRebelsLedBy leader)
                        return (" (" <> mtext <> ")")
                    Nothing -> return ""
                progressText <- case sr_unrest reb of
                    Just unrest -> do
                        mtext <- messageText (MsgRebelsGainProgress unrest)
                        return (" (" <> mtext <> ")")
                    Nothing -> return ""
                return $ MsgSpawnRebels
                            (maybe "" (\(ty, ty_icon) -> iconText ty_icon <> " " <> ty) rtype_loc_icon)
                            size
                            friendText
                            leaderText
                            (win reb)
                            progressText
            _ -> return $ preMessage stmt

hasSpawnedRebels :: (IsGameState (GameState g), Monad m) => StatementHandler g m
hasSpawnedRebels [pdx| %_ = $rtype |]
    | Just (rtype_loc, rtype_iconkey) <- HM.lookup rtype rebel_loc
      = msgToPP $ MsgRebelsHaveRisen (iconText rtype_iconkey) rtype_loc
hasSpawnedRebels stmt = preStatement stmt

canSpawnRebels :: (IsGameState (GameState g), Monad m) => StatementHandler g m
canSpawnRebels [pdx| %_ = $rtype |]
    | Just (rtype_loc, rtype_iconkey) <- HM.lookup rtype rebel_loc
      = msgToPP (MsgProvinceHasRebels (iconText rtype_iconkey) rtype_loc)
canSpawnRebels stmt = preStatement stmt

-- Events

data TriggerEvent = TriggerEvent
        { e_id :: Maybe Text
        , e_title_loc :: Maybe Text
        , e_days :: Maybe Double
        }
newTriggerEvent :: TriggerEvent
newTriggerEvent = TriggerEvent Nothing Nothing Nothing
triggerEvent :: forall g m. (EU4Info g, Monad m) => ScriptMessage -> StatementHandler g m
triggerEvent evtType stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_trigger_event =<< foldM addLine newTriggerEvent scr
    where
        addLine :: TriggerEvent -> GenericStatement -> PPT g m TriggerEvent
        addLine evt [pdx| id = ?!eeid |]
            | Just eid <- either (\n -> T.pack (show (n::Int))) id <$> eeid
            = do
                mevt_t <- getEventTitle eid
                return evt { e_id = Just eid, e_title_loc = mevt_t }
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

-- Specific values

gainMen :: forall g m. (IsGameState (GameState g), Monad m) => StatementHandler g m
gainMen [pdx| $head = !amt |]
    | "add_manpower" <- head = gainMen' ("manpower"::Text) MsgGainMPFrac MsgGainMP 1000
    | "add_sailors" <- head = gainMen' ("sailors"::Text) MsgGainSailorsFrac MsgGainSailors 1
    where
        gainMen' theicon msgFrac msgWhole mult = msgToPP =<<
            if abs (amt::Double) < 1
            --  interpret amt as a fraction of max
            then return $ msgFrac theicon amt
            --  interpret amt as exact number, multiplied by mult
            else return $ msgWhole theicon (amt*mult)
gainMen stmt = preStatement stmt

-- Casus belli

data AddCB = AddCB
    {   acb_target_flag :: Maybe Text
    ,   acb_type :: Maybe Text
    ,   acb_type_loc :: Maybe Text
    ,   acb_months :: Maybe Double
    }
newAddCB :: AddCB
newAddCB = AddCB Nothing Nothing Nothing Nothing
addCB :: forall g m. (EU4Info g, Monad m) =>
    Bool -- ^ True for add_casus_belli, False for reverse_add_casus_belli
        -> StatementHandler g m
addCB direct stmt@[pdx| %_ = @scr |]
    = msgToPP . pp_add_cb =<< foldM addLine newAddCB scr where
        addLine :: AddCB -> GenericStatement -> PPT g m AddCB
        addLine acb [pdx| target = $target |]
            = (\target_loc -> acb
                  { acb_target_flag = target_loc })
              <$> eflag (Just EU4Country) (Left target)
        addLine acb [pdx| target = $vartag:$var |]
            = (\target_loc -> acb
                  { acb_target_flag = target_loc })
              <$> eflag (Just EU4Country) (Right (vartag, var))
        addLine acb [pdx| type = $cbtype |]
            = (\cbtype_loc -> acb
                  { acb_type = Just cbtype
                  , acb_type_loc = cbtype_loc })
              <$> getGameL10nIfPresent cbtype
        addLine acb [pdx| months = %rhs |]
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

random :: (EU4Info g, Monad m) => StatementHandler g m
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

randomList :: (EU4Info g, Monad m) => StatementHandler g m
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
randomList _ = withCurrentFile $ \file ->
    error ("randomList sent strange statement in " ++ file)

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
newDefineAdvisor :: DefineAdvisor
newDefineAdvisor = DefineAdvisor Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

defineAdvisor :: forall g m. (IsGameData (GameData g),
                              IsGameState (GameState g),
                              Monad m) => StatementHandler g m
defineAdvisor stmt@[pdx| %_ = @scr |]
    = msgToPP . pp_define_advisor =<< foldM addLine newDefineAdvisor scr where
        addLine :: DefineAdvisor -> GenericStatement -> PPT g m DefineAdvisor
        addLine da [pdx| $lhs = %rhs |] = case T.map toLower lhs of
            "type" ->
                let mthe_type = case rhs of
                        GenericRhs a_type [] -> Just a_type
                        StringRhs a_type -> Just a_type
                        _ -> Nothing
                in (\mtype_loc -> da
                        { da_type = mthe_type
                        , da_type_loc = mtype_loc })
                   <$> maybe (return Nothing) getGameL10nIfPresent mthe_type
            "name" -> return $
                let mthe_name = case rhs of
                        GenericRhs a_name [] -> Just a_name
                        StringRhs a_name -> Just a_name
                        _ -> Nothing
                in da { da_name = mthe_name }
            "discount" -> return $
                let yn = case rhs of
                        GenericRhs yn' [] -> Just yn'
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
                        GenericRhs yn' [] -> Just yn'
                        StringRhs yn' -> Just yn'
                        _ -> Nothing
                in if yn == Just "yes" then da { da_female = Just True }
                   else if yn == Just "no" then da { da_female = Just False }
                   else da
            param -> trace ("warning: unknown define_advisor parameter: " ++ show param) $ return da
        addLine da _ = return da
        pp_define_advisor :: DefineAdvisor -> ScriptMessage
        pp_define_advisor da =
            case da_skill da of
                Just skill ->
                    let mdiscount = da_discount da
                        discount = fromMaybe False mdiscount
                        mlocation_loc = da_location_loc da
                        mlocation = mlocation_loc `mplus` (T.pack . show <$> da_location da)
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
                _ -> preMessage stmt
defineAdvisor stmt = preStatement stmt

-- Rulers

data Dynasty
    = DynText Text
    | DynPron Text
    | DynOriginal
    | DynHistoric

data DefineRuler = DefineRuler
    {   dr_rebel :: Bool
    ,   dr_name :: Maybe Text
    ,   dr_dynasty :: Maybe Dynasty
    ,   dr_age :: Maybe Double
    ,   dr_female :: Maybe Bool
    ,   dr_claim :: Maybe Double
    ,   dr_regency :: Bool
    ,   dr_adm :: Maybe Int
    ,   dr_dip :: Maybe Int
    ,   dr_mil :: Maybe Int
    ,   dr_fixed :: Bool
    ,   dr_culture :: Maybe (Either Text Text)
    ,   dr_religion :: Maybe (Either Text Text)
    ,   dr_attach_leader :: Maybe Text
    }
newDefineRuler :: DefineRuler
newDefineRuler = DefineRuler False Nothing Nothing Nothing Nothing Nothing False Nothing Nothing Nothing False Nothing Nothing Nothing

defineRuler :: forall g m. (EU4Info g, Monad m) => StatementHandler g m
defineRuler [pdx| %_ = @scr |] = do
    -- Since addLine is pure, we have to prepare these in advance in case we
    -- need them.
    prevPronoun <- Doc.doc2text <$> pronoun Nothing "PREV"
    rootPronoun <- Doc.doc2text <$> pronoun Nothing "ROOT"
    thisPronoun <- Doc.doc2text <$> pronoun Nothing "THIS"
    hrePronoun  <- Doc.doc2text <$> pronoun Nothing "emperor" -- needs l10n
    let testPronoun :: Maybe Text -> Maybe (Either Text Text)
        testPronoun (Just "PREV") = Just (Right prevPronoun)
        testPronoun (Just "ROOT") = Just (Right rootPronoun)
        testPronoun (Just "THIS") = Just (Right thisPronoun)
        testPronoun (Just "emperor") = Just (Right hrePronoun)
        testPronoun (Just other) = Just (Left other)
        testPronoun _ = Nothing

        addLine :: DefineRuler -> GenericStatement -> DefineRuler
        addLine dr [pdx| $lhs = %rhs |] = case T.map toLower lhs of
            "rebel" -> case textRhs rhs of
                Just "yes" -> dr { dr_rebel = True }
                _ -> dr
            "name" -> dr { dr_name = textRhs rhs }
            "dynasty" -> dr { dr_dynasty = case testPronoun $ textRhs rhs of
                Just (Right pronoun) -> Just (DynPron pronoun)
                Just (Left "original_dynasty") -> Just DynOriginal
                Just (Left "historic_dynasty") -> Just DynHistoric
                Just (Left other) -> Just (DynText other)
                _ -> Nothing }
            "age" -> dr { dr_age = floatRhs rhs }
            "female" -> case textRhs rhs of
                Just "yes" -> dr { dr_female = Just True }
                Just "no"  -> dr { dr_female = Just False }
                _ -> dr
            "claim" -> dr { dr_claim = floatRhs rhs }
            "regency" -> case textRhs rhs of
                Just "yes" -> dr { dr_regency = True }
                _ -> dr
            "adm" -> dr { dr_adm = floatRhs rhs }
            "dip" -> dr { dr_dip = floatRhs rhs }
            "mil" -> dr { dr_mil = floatRhs rhs }
            "fixed" -> case textRhs rhs of
                Just "yes" -> dr { dr_fixed = True }
                _ -> dr
            "culture" -> dr { dr_culture = testPronoun $ textRhs rhs }
            "religion" -> dr { dr_religion = testPronoun $ textRhs rhs }
            "attach_leader" -> dr { dr_attach_leader = textRhs rhs }
            param -> trace ("warning: unknown define_ruler parameter: " ++ show param) $ dr
        addLine dr _ = dr

        pp_define_ruler :: DefineRuler -> PPT g m IndentedMessages
        pp_define_ruler    DefineRuler { dr_rebel = True } = msgToPP MsgRebelLeaderRuler
        pp_define_ruler dr@DefineRuler { dr_regency = regency, dr_attach_leader = mleader } = do
            body <- indentUp (unfoldM pp_define_ruler_attrib dr)
            if null body then
                msgToPP (maybe (MsgNewRuler regency) (MsgNewRulerLeader regency) mleader)
            else
                liftA2 (++)
                    (msgToPP (maybe (MsgNewRulerAttribs regency) (MsgNewRulerLeaderAttribs regency) mleader))
                    (pure body)
        pp_define_ruler_attrib :: DefineRuler -> PPT g m (Maybe (IndentedMessage, DefineRuler))
        -- "Named <foo>"
        pp_define_ruler_attrib dr@DefineRuler { dr_name = Just name } = do
            [msg] <- msgToPP (MsgNewRulerName name)
            return (Just (msg, dr { dr_name = Nothing }))
        -- "Of the <foo> dynasty"
        pp_define_ruler_attrib dr@DefineRuler { dr_dynasty = Just dynasty } =
            case dynasty of
                DynText dyntext -> do
                    [msg] <- msgToPP (MsgNewRulerDynasty dyntext)
                    return (Just (msg, dr { dr_dynasty = Nothing }))
                DynPron dyntext -> do
                    [msg] <- msgToPP (MsgNewRulerDynastyAs dyntext)
                    return (Just (msg, dr { dr_dynasty = Nothing }))
                DynOriginal -> do
                    [msg] <- msgToPP MsgNewRulerOriginalDynasty
                    return (Just (msg, dr { dr_dynasty = Nothing }))
                DynHistoric -> do
                    [msg] <- msgToPP MsgNewRulerHistoricDynasty
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
        -- "Claim strength <foo>"
        pp_define_ruler_attrib dr@DefineRuler { dr_claim = Just claim } = do
            [msg] <- msgToPP $ MsgNewRulerClaim claim
            return (Just (msg, dr { dr_claim = Nothing }))
        -- "Of the <foo> culture"
        pp_define_ruler_attrib dr@DefineRuler { dr_culture = Just culture } = case culture of
            Left cultureText -> do
              locCulture <- getGameL10n cultureText
              [msg] <- msgToPP $ MsgNewRulerCulture locCulture
              return (Just (msg, dr { dr_culture = Nothing }))
            Right cultureText -> do
              [msg] <- msgToPP $ MsgNewRulerCultureAs cultureText
              return (Just (msg, dr { dr_culture = Nothing }))
        -- "Following the <foo> religion"
        pp_define_ruler_attrib dr@DefineRuler { dr_religion = Just religion } = case religion of
            Left religionText -> do
              locReligion <- getGameL10n religionText
              [msg] <- msgToPP $ MsgNewRulerReligion (iconText religionText) locReligion
              return (Just (msg, dr { dr_religion = Nothing }))
            Right religionText -> do
              [msg] <- msgToPP $ MsgNewRulerReligionAs religionText
              return (Just (msg, dr { dr_religion = Nothing }))
        -- Nothing left
        pp_define_ruler_attrib _ = return Nothing
    pp_define_ruler $ foldl' addLine newDefineRuler scr
defineRuler stmt = preStatement stmt

-- Building units

data UnitType
    = UnitInfantry
    | UnitCavalry
    | UnitArtillery
    | UnitHeavyShip
    | UnitLightShip
    | UnitGalley
    | UnitTransport
    deriving (Show)

instance Param UnitType where
    toParam (textRhs -> Just "heavy_ship") = Just UnitHeavyShip
    toParam (textRhs -> Just "light_ship") = Just UnitLightShip
    toParam (textRhs -> Just "galley")     = Just UnitGalley
    toParam (textRhs -> Just "transport")  = Just UnitTransport
    toParam _ = Nothing

--buildToForcelimit :: (IsGameState (GameState g), Monad m) => StatementHandler g m
foldCompound "buildToForcelimit" "BuildToForcelimit" "btf"
    []
    [CompField "infantry" [t|Double|] (Just [|0|]) False
    ,CompField "cavalry" [t|Double|] (Just [|0|]) False
    ,CompField "artillery" [t|Double|] (Just [|0|]) False
    ,CompField "heavy_ship" [t|Double|] (Just [|0|]) False
    ,CompField "light_ship" [t|Double|] (Just [|0|]) False
    ,CompField "galley" [t|Double|] (Just [|0|]) False
    ,CompField "transport" [t|Double|] (Just [|0|]) False
    ]
    [| let has_infantry = _infantry > 0
           has_cavalry = _cavalry > 0
           has_artillery = _artillery > 0
           has_heavy_ship = _heavy_ship > 0
           has_light_ship = _light_ship > 0
           has_galley = _galley > 0
           has_transport = _transport > 0
           has_land = has_infantry || has_cavalry || has_artillery
           has_navy = has_heavy_ship || has_light_ship || has_galley || has_transport
       in if has_land == has_navy then
                -- Neither or both. Unlikely, not provided for
                preMessage stmt
            else if has_land then
                let infIcon = iconText "infantry"
                    cavIcon = iconText "cavalry"
                    artIcon = iconText "artillery"
                in MsgBuildToForcelimitLand infIcon _infantry
                                            cavIcon _cavalry
                                            artIcon _artillery
            else -- has_navy == True
                let heavyIcon = iconText "heavy ship"
                    lightIcon = iconText "light ship"
                    gallIcon = iconText "galley"
                    transpIcon = iconText "transport"
                in MsgBuildToForcelimitNavy heavyIcon _heavy_ship
                                            lightIcon _light_ship
                                            gallIcon _galley
                                            transpIcon _transport
    |]

--addUnitConstruction :: (IsGameState (GameState g), Monad m) => Text -> StatementHandler g m
foldCompound "addUnitConstruction" "UnitConstruction" "uc"
    [("extraArg", [t|Text|])]
    [CompField "amount" [t|Double|] Nothing True
    ,CompField "type" [t|UnitType|] Nothing True
    ,CompField "speed" [t|Double|] (Just [|1|]) False
    ,CompField "cost" [t|Double|] (Just [|1|]) False
    ,CompField "optionalArg" [t|Double|] Nothing False]
    [| (case _type of
            UnitHeavyShip -> MsgBuildHeavyShips (iconText "heavy ship")
            UnitLightShip -> MsgBuildLightShips (iconText "light ship")
            UnitGalley    -> MsgBuildGalleys    (iconText "galley")
            UnitTransport -> MsgBuildTransports (iconText "transport")
       ) _amount _speed _cost
    |]

-- War

data DeclareWarWithCB = DeclareWarWithCB
    {   dwcb_who :: Maybe Text
    ,   dwcb_cb :: Maybe Text
    }
newDeclareWarWithCB :: DeclareWarWithCB
newDeclareWarWithCB = DeclareWarWithCB Nothing Nothing

declareWarWithCB :: forall g m. (EU4Info g, Monad m) => StatementHandler g m
declareWarWithCB stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_declare_war_with_cb (foldl' addLine newDeclareWarWithCB scr) where
        addLine :: DeclareWarWithCB -> GenericStatement -> DeclareWarWithCB
        addLine dwcb [pdx| $lhs = $rhs |]
            = case T.map toLower lhs of
                "who"         -> dwcb { dwcb_who = Just rhs }
                "casus_belli" -> dwcb { dwcb_cb  = Just rhs }
                _ -> dwcb
        addLine dwcb _ = dwcb
        pp_declare_war_with_cb :: DeclareWarWithCB -> PPT g m ScriptMessage
        pp_declare_war_with_cb dwcb
              = case (dwcb_who dwcb, dwcb_cb dwcb) of
                (Just who, Just cb) -> do
                    whoflag <- Doc.doc2text <$> flag (Just EU4Country) who
                    cb_loc <- getGameL10n cb
                    return (MsgDeclareWarWithCB whoflag cb_loc)
                _ -> return $ preMessage stmt
declareWarWithCB stmt = preStatement stmt

-- DLC

hasDlc :: (IsGameState (GameState g), Monad m) => StatementHandler g m
hasDlc [pdx| %_ = ?dlc |]
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
            ,("Mare Nostrum", "mn")
            ,("Rights of Man", "rom")
            ,("Mandate of Heaven", "moh")
            ,("Third Rome", "tr")
            ,("Cradle of Civilization", "coc")
            ,("Rule Britannia", "rb")
            ,("Dharma", "dhr")
            ]
        dlc_icon = maybe "" iconText mdlc_key
hasDlc stmt = preStatement stmt

-- Estates

data EstateInfluenceModifier = EstateInfluenceModifier {
        eim_estate :: Maybe Text
    ,   eim_modifier :: Maybe Text
    }
newEIM :: EstateInfluenceModifier
newEIM = EstateInfluenceModifier Nothing Nothing
hasEstateInfluenceModifier :: (IsGameData (GameData g),
                               IsGameState (GameState g),
                               Monad m) => StatementHandler g m
hasEstateInfluenceModifier stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_eim (foldl' addField newEIM scr)
    where
        addField :: EstateInfluenceModifier -> GenericStatement -> EstateInfluenceModifier
        addField inf [pdx| estate   = $est      |] = inf { eim_estate = Just est }
        addField inf [pdx| modifier = $modifier |] = inf { eim_modifier = Just modifier }
        addField inf _ = inf -- unknown statement
        pp_eim inf = case (eim_estate inf, eim_modifier inf) of
            (Just est, Just modifier) -> do
                loc_est <- getGameL10n est
                loc_mod <- getGameL10n modifier
                return $ MsgEstateHasInfluenceModifier (iconText est) loc_est loc_mod
            _ -> return (preMessage stmt)
hasEstateInfluenceModifier stmt = preStatement stmt

data AddEstateInfluenceModifier = AddEstateInfluenceModifier {
        aeim_estate :: Maybe Text
    ,   aeim_desc :: Maybe Text
    ,   aeim_influence :: Maybe Double
    ,   aeim_duration :: Maybe Double
    } deriving Show
newAddEstateInfluenceModifier :: AddEstateInfluenceModifier
newAddEstateInfluenceModifier = AddEstateInfluenceModifier Nothing Nothing Nothing Nothing

timeOrIndef :: (IsGameData (GameData g), Monad m) => Double -> PPT g m Text
timeOrIndef n = if n < 0 then messageText MsgIndefinitely else messageText (MsgForDays n)

estateInfluenceModifier :: forall g m. (IsGameData (GameData g),
                                        IsGameState (GameState g),
                                        Monad m) =>
    (Text -> Text -> Text -> Double -> Text -> ScriptMessage)
        -> StatementHandler g m
estateInfluenceModifier msg stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_eim (foldl' addLine newAddEstateInfluenceModifier scr)
    where
        addLine :: AddEstateInfluenceModifier -> GenericStatement -> AddEstateInfluenceModifier
        addLine aeim [pdx| estate    = $estate   |] = aeim { aeim_estate = Just estate }
        addLine aeim [pdx| desc      = $desc     |] = aeim { aeim_desc = Just desc }
        addLine aeim [pdx| influence = !inf      |] = aeim { aeim_influence = Just inf }
        addLine aeim [pdx| duration  = !duration |] = aeim { aeim_duration = Just duration }
        addLine aeim _ = aeim
        pp_eim :: AddEstateInfluenceModifier -> PPT g m ScriptMessage
        pp_eim aeim
            = case (aeim_estate aeim, aeim_desc aeim, aeim_influence aeim, aeim_duration aeim) of
                (Just estate, Just desc, Just inf, Just duration) -> do
                    let estate_icon = iconText estate
                    estate_loc <- getGameL10n estate
                    desc_loc <- getGameL10n desc
                    dur <- timeOrIndef duration
                    return (msg estate_icon estate_loc desc_loc inf dur)
                _ -> return (preMessage stmt)
estateInfluenceModifier _ stmt = preStatement stmt

-- Trigger switch

triggerSwitch :: (EU4Info g, Monad m) => StatementHandler g m
-- A trigger switch must be of the form
-- trigger_switch = {
--  on_trigger = <statement lhs>
--  <statement rhs> = {
--      <actions>
--  }
-- }
-- where the <statement rhs> block may be repeated several times.
triggerSwitch stmt@(Statement _ OpEq (CompoundRhs
                    ([pdx| on_trigger = $condlhs |] -- assume this is first statement
                    :clauses))) = do
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

-- | Handle @calc_true_if@ clauses, of the following form:
-- 
-- @
--  calc_true_if = {
--       <conditions>
--       amount = N
--  }
-- @
--
-- This tests the conditions, and returns true if at least N of them are true.
-- They can be individual conditions, e.g. from @celestial_empire_events.3@:
--
-- @
--  calc_true_if = {
--      accepted_culture = manchu
--      accepted_culture = chihan
--      accepted_culture = miao
--      accepted_culture = cantonese
--      ... etc. ...
--      amount = 2
--   }
-- @
--
-- or a single "all" scope, e.g. from @court_and_country_events.3@:
--
-- @
--  calc_true_if = {
--      all_core_province = {
--          owned_by = ROOT
--          culture = PREV
--      }
--      amount = 5
--  }
-- @
--
-- We expect the @amount@ clause to come last.
calcTrueIf :: (EU4Info g, Monad m) => StatementHandler g m
calcTrueIf stmt@[pdx| %_ = @stmts |]
    | Just (conds, [pdx| amount = !count |]) <- unsnoc stmts
    = do
        stmtMessages <- ppMany conds
        withCurrentIndent $ \i ->
            return $ (i, MsgCalcTrueIf count) : stmtMessages
calcTrueIf stmt = preStatement stmt

-- Heirs

data Heir = Heir
        {   heir_dynasty :: Maybe Text
        ,   heir_claim :: Maybe Double
        ,   heir_age :: Maybe Double
        }
newHeir :: Heir
newHeir = Heir Nothing Nothing Nothing
defineHeir :: forall g m. (EU4Info g, Monad m) => StatementHandler g m
defineHeir [pdx| %_ = @scr |]
    = msgToPP =<< pp_heir (foldl' addLine newHeir scr)
    where
        addLine :: Heir -> GenericStatement -> Heir
        addLine heir [pdx| dynasty = $dynasty |] = heir { heir_dynasty = Just dynasty }
        addLine heir [pdx| claim   = !claim   |] = heir { heir_claim = Just claim }
        addLine heir [pdx| age     = !age     |] = heir { heir_age = Just age }
        addLine heir _ = heir
        pp_heir :: IsGameData (GameData g) => Heir -> PPT g m ScriptMessage
        pp_heir heir = do
            dynasty_flag <- fmap Doc.doc2text <$> maybeM (flag (Just EU4Country)) (heir_dynasty heir)
            case (heir_age heir, dynasty_flag, heir_claim heir) of
                (Nothing,  Nothing,   Nothing)     -> return $ MsgNewHeir
                (Nothing,  Nothing,   Just claim)  -> return $ MsgNewHeirClaim claim
                (Nothing,  Just cflag, Nothing)     -> return $ MsgNewHeirDynasty cflag
                (Nothing,  Just cflag, Just claim)  -> return $ MsgNewHeirDynastyClaim cflag claim
                (Just age, Nothing,   Nothing)     -> return $ MsgNewHeirAge age
                (Just age, Nothing,   Just claim)  -> return $ MsgNewHeirAgeClaim age claim
                (Just age, Just cflag, Nothing)    -> return $ MsgNewHeirAgeFlag age cflag
                (Just age, Just cflag, Just claim) -> return $ MsgNewHeirAgeFlagClaim age cflag claim
defineHeir stmt = preStatement stmt

-- Holy Roman Empire

-- Assume 1 <= n <= 8
hreReformLoc :: (IsGameData (GameData g), Monad m) => Int -> PPT g m Text
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

hreReformLevel :: (IsGameData (GameData g),
                   IsGameState (GameState g),
                   Monad m) => StatementHandler g m
hreReformLevel [pdx| %_ = !level |] | level >= 0, level <= 8
    = if level == 0
        then msgToPP MsgNoHREReforms
        else msgToPP . MsgHREPassedReform =<< hreReformLoc level
hreReformLevel stmt = preStatement stmt

-- Religion

religionYears :: (IsGameData (GameData g),
                  IsGameState (GameState g),
                  Monad m) => StatementHandler g m
religionYears [pdx| %_ = { $rel = !years } |]
    = do
        let rel_icon = iconText rel
        rel_loc <- getGameL10n rel
        msgToPP $ MsgReligionYears rel_icon rel_loc years
religionYears stmt = preStatement stmt

-- Government

govtRank :: (IsGameState (GameState g), Monad m) => StatementHandler g m
govtRank [pdx| %_ = !level |]
    = case level :: Int of
        1 -> msgToPP MsgRankDuchy -- unlikely, but account for it anyway
        2 -> msgToPP MsgRankKingdom
        3 -> msgToPP MsgRankEmpire
        _ -> error "impossible: govtRank matched an invalid rank number"
govtRank stmt = preStatement stmt

setGovtRank :: (IsGameState (GameState g), Monad m) => StatementHandler g m
setGovtRank [pdx| %_ = !level |] | level `elem` [1..3]
    = case level :: Int of
        1 -> msgToPP MsgSetRankDuchy
        2 -> msgToPP MsgSetRankKingdom
        3 -> msgToPP MsgSetRankEmpire
        _ -> error "impossible: setGovtRank matched an invalid rank number"
setGovtRank stmt = preStatement stmt

numProvinces :: (IsGameData (GameData g),
                 IsGameState (GameState g),
                 Monad m) =>
    Text
        -> (Text -> Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numProvinces micon msg [pdx| $what = !amt |] = do
    what_loc <- getGameL10n what
    msgToPP (msg (iconText micon) what_loc amt)
numProvinces _ _ stmt = preStatement stmt

withFlagOrProvince :: (EU4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
withFlagOrProvince countryMsg _ stmt@[pdx| %_ = ?_ |]
    = withFlag countryMsg stmt
withFlagOrProvince countryMsg _ stmt@[pdx| %_ = $_:$_ |]
    = withFlag countryMsg stmt -- could be either
withFlagOrProvince _ provinceMsg stmt@[pdx| %_ = !(_ :: Double) |]
    = withProvince provinceMsg stmt
withFlagOrProvince _ _ stmt = preStatement stmt

withFlagOrProvinceEU4Scope :: (EU4Info g, Monad m) =>
    (Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
withFlagOrProvinceEU4Scope bothCountryMsg scopeCountryParamGeogMsg scopeGeogParamCountryMsg bothGeogMsg stmt = do
    mscope <- getCurrentScope
    -- If no scope, assume country.
    if fromMaybe False (isGeographic <$> mscope) then
        withFlagOrProvince scopeGeogParamCountryMsg bothGeogMsg stmt
    else
        withFlagOrProvince bothCountryMsg scopeCountryParamGeogMsg stmt

tradeMod :: (EU4Info g, Monad m) => StatementHandler g m
tradeMod stmt@[pdx| %_ = ?_ |]
    = withLocAtom2 MsgTradeMod MsgHasModifier stmt
tradeMod stmt@[pdx| %_ = @_ |]
    = textAtom "who" "name" MsgHasTradeModifier
        (fmap Just . flagText (Just EU4Country))
        stmt
tradeMod stmt = preStatement stmt

isMonth :: (IsGameData (GameData g),
            IsGameState (GameState g),
            Monad m) => StatementHandler g m
isMonth [pdx| %_ = !(num :: Int) |] | num >= 1, num <= 12
    = do
        month_loc <- getGameL10n $ case num of
            0 -> "January" -- programmer counting -_-
            1 -> "February"
            2 -> "March"
            3 -> "April"
            4 -> "May"
            5 -> "June"
            6 -> "July"
            7 -> "August"
            8 -> "September"
            9 -> "October"
            10 -> "November"
            11 -> "December"
            _ -> error "impossible: tried to localize bad month number"
        msgToPP $ MsgIsMonth month_loc
isMonth stmt = preStatement stmt

range :: (EU4Info g, Monad m) => StatementHandler g m
range stmt@[pdx| %_ = !(_ :: Double) |]
    = numericIcon "colonial range" MsgGainColonialRange stmt
range stmt = withFlag MsgIsInColonialRange stmt

area :: (EU4Info g, Monad m) => StatementHandler g m
area stmt@[pdx| %_ = @_ |] = scope EU4Geographic $ compoundMessage MsgArea stmt
area stmt                  = locAtomTagOrProvince (const MsgAreaIs) MsgAreaIsAs stmt

-- Currently dominant_culture only appears in decisions/Cultural.txt
-- (dominant_culture = capital).
dominantCulture :: (IsGameState (GameState g), Monad m) => StatementHandler g m
dominantCulture [pdx| %_ = capital |] = msgToPP MsgCapitalCultureDominant
dominantCulture stmt = preStatement stmt

customTriggerTooltip :: (EU4Info g, Monad m) => StatementHandler g m
customTriggerTooltip [pdx| %_ = @scr |]
    -- ignore the custom tooltip
    = let rest = flip filter scr $ \stmt -> case stmt of
            [pdx| tooltip = %_ |] -> False
            _ -> True
      in indentDown $ ppMany rest
customTriggerTooltip stmt = preStatement stmt

piety :: (IsGameState (GameState g), Monad m) => StatementHandler g m
piety stmt@[pdx| %_ = !amt |]
    = numericIcon (case amt `compare` (0::Double) of
        LT -> "lack of piety"
        _  -> "being pious")
      MsgPiety stmt
piety stmt = preStatement stmt

----------------------
-- Idea group ideas --
----------------------

hasIdea :: (EU4Info g, Monad m) =>
    (Text -> Int -> ScriptMessage)
        -> StatementHandler g m
hasIdea msg stmt@[pdx| $lhs = !n |] | n >= 1, n <= 7 = do
    groupTable <- getIdeaGroups
    let mideagroup = HM.lookup lhs groupTable
    case mideagroup of
        Nothing -> preStatement stmt -- unknown idea group
        Just grp -> do
            let idea = ig_ideas grp !! (n - 1)
                ideaKey = idea_name idea
            idea_loc <- getGameL10n ideaKey
            msgToPP (msg idea_loc n)
hasIdea _ stmt = preStatement stmt

-----------
-- Trust --
-----------

data Trust = Trust
        {   tr_whom :: Maybe Text
        ,   tr_amount :: Maybe Double
        ,   tr_mutual :: Bool
        }
newTrust :: Trust
newTrust = Trust Nothing Nothing False
trust :: forall g m. (EU4Info g, Monad m) => StatementHandler g m
trust stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_trust =<< foldM addLine newTrust scr
    where
        addLine :: Trust -> GenericStatement -> PPT g m Trust
        addLine tr [pdx| who = $whom |] = do
            whom' <- Doc.doc2text <$> pronoun (Just EU4Country) whom
            return tr { tr_whom = Just whom' }
        addLine tr [pdx| value = !amt |]
            = return tr { tr_amount = Just amt }
        addLine tr [pdx| mutual = yes |]
            = return tr { tr_mutual = True }
        addLine tr _ = return tr
        pp_trust tr
            | (Just whom, Just amt) <- (tr_whom tr, tr_amount tr)
              = return $ (if tr_mutual tr then MsgAddTrustMutual else MsgAddTrust)
                          whom amt
            | otherwise = return (preMessage stmt)
trust stmt = preStatement stmt

----------------------------------------
-- Government form-specific mechanics --
----------------------------------------

-- Currently this form only affects Russian government.

gpMechanicTable :: HashMap (Text, MonarchPower) (Double -> ScriptMessage)
gpMechanicTable = HM.fromList
    [(("russian_mechanic", Administrative), MsgSudebnikProgress)
    ,(("russian_mechanic", Diplomatic), MsgOprichninaProgress)
    ,(("russian_mechanic", Military), MsgStreltsyProgress)
    ]

data GovernmentPower = GovernmentPower
        {   gp_mechanic :: Maybe Text
        ,   gp_category :: Maybe MonarchPower
        ,   gp_amount :: Maybe Double
        }
newGP :: GovernmentPower
newGP = GovernmentPower Nothing Nothing Nothing
governmentPower :: (IsGameData (GameData g),
                    IsGameState (GameState g),
                    Monad m) => StatementHandler g m
governmentPower stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_gp (foldl' addLine newGP scr)
    where
        addLine :: GovernmentPower -> GenericStatement -> GovernmentPower
        addLine gp [pdx| government_mechanic = $mechanic |]
            = gp { gp_mechanic = Just mechanic }
        addLine gp [pdx| which = $cat      |]
            = case cat of
                "ADM" -> gp { gp_category = Just Administrative }
                "DIP" -> gp { gp_category = Just Diplomatic }
                "MIL" -> gp { gp_category = Just Military }
                _ -> gp
        addLine gp [pdx| amount = !amt |]
            = gp { gp_amount = Just amt }
        addLine gp _ = gp
        pp_gp gp
            | (Just mech, Just cat, Just amt) <- (gp_mechanic gp, gp_category gp, gp_amount gp),
              Just powmsg <- HM.lookup (mech, cat) gpMechanicTable
              = return (powmsg amt)
            | otherwise = return (preMessage stmt)
governmentPower stmt = preStatement stmt
