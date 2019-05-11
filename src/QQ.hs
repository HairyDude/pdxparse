{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
Module      : QQ
Description : Quasiquoter for Clausewitz script (values and patterns)

The 'pdx' quasiquoter allows literal scripts to substitute for the script
data structures. To interpolate or bind variables, it uses the following
syntax. (@foo@ is either a variable name or a parenthesised Haskell
expression. If it's a Haskell expression, it can't be a TH splice or
quasiquote.)

In an expression:

    * @$foo@ => @foo@ is a bare_word (Also makes sense on LHS)

    e.g. if the variable @bar@ contains the string @\"bar\"@, then
    @
        [pdx|foo = $bar|] => foo = bar
    @

    * @%foo@ => @foo@ is the relevant script type

    e.g. if the variable @bar@ contains @GenericRhs \"bar\"@, then
    @
        [pdx|%bar|] => type error
        [pdx|foo = %bar|] => foo = bar
        [pdx|foo = %(case bar of GenericRhs s -> StringRhs (s <> \"quux\"))|]
            => foo = \"barquux\"
    @

    * @?foo@ => \"<contents of foo>\" (where foo :: Text)

    e.g. if the variable @bar@ contains @\"bar\"@, then
    @
        [pdx|foo = ?bar|] => foo = "bar"
    @

    * @!foo@ => @<contents of foo>@ (where @foo :: Int@)

    e.g. if the variable @bar@ contains @23@, then
    @
        [pdx|foo = !bar|] => foo = 23
    @

    @@foo@ => @foo@ is a compound RHS

    e.g. if the variable @bar@ contains @[scr1, scr2]@,
    @
        [pdx|foo = @bar|] => foo = { %scr1 %scr2 }
    @

In a pattern:

    * on the LHS or RHS:

        * @%foo@ => @foo@ is bound as the relevant script type. For example:
        @
            [pdx|%foo|] => foo :: GenericScript
            [pdx|foo = %foo|] => foo :: GenericRhs
        @

        * @$foo@ => @foo :: Text@, matches a bare_word
        
        * @!foo@ => @foo :: Int@, matches a number (goes through @floatRhs@)

    * on the RHS only:

        * @?!foo@ => @foo :: 'CoerceNum' a => Either a String@, matches a
        number, a bare_word, or a \"string\"

        * @?foo@ => @foo :: Text@, matches a \"string\" /or/ a bare_word (goes
        through 'textRhs')

        * @@foo@ => @foo :: GenericScript@, matches a compound

Excess whitespace is allowed before and after the expression and either side of
the operator.

Currently, the quasiquoter has no syntax for a variable operator or (except via
the generic % interpolator) variable date literals.
-}

module QQ (
        pdx     -- :: QuasiQuoter
    ) where

import Control.Applicative (Applicative (..), Alternative (..))
import Control.Monad (forM, guard)
import Data.Monoid ((<>))
import Data.Either (partitionEithers)
import Data.Maybe (isNothing)

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Ap
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString (..))

import Language.Haskell.TH -- everything
import qualified Language.Haskell.TH.Lift as TL
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Instances.TH.Lift ()
--import Language.Haskell.Exts.QQ
import Language.Haskell.Meta (parseExp, parsePat)

import Abstract

-------------
-- Helpers --
-------------

-- | Haskell code or string literal?
data StringOrCode
    = StringNotCode String
    | CodeNotString String
    deriving (Show, Eq, Ord)

-----------------
-- Expressions --
-----------------

-- | Statement components. Each constructor corresponds with one possible thing
-- on a LHS or RHS. Not all constructors actually make sense in both contexts;
-- e.g. 'ExpHsCompound' doesn't make sense on the left.
data StExp
    = ExpHsDirect String    -- %foo => foo :: <inferred type>
    | ExpHsGeneric StringOrCode [StringOrCode]
                            -- $foo => Generic[LR]hs foo []
                            -- $foo:bar => Generic[LR]hs (CodeNotString "foo") [StringNotCode "bar"]
                            -- foo:$bar => Generic[LR]hs (StringNotCode "foo") [CodeNotString "bar"]
                            -- $foo:$bar:baz =>
                            --      Generic[LR]hs (CodeNotString "foo") [CodeNotString "bar", StringNotCode "baz"]
    | ExpHsString String    -- ?foo => StringRhs foo
    | ExpHsInt String       -- !foo => IntRhs foo
    | ExpHsCompound String  -- @foo => CompoundRhs foo
    deriving (Show, Eq, Ord)
    -- TODO: dates
type StExpL = StExp
type StExpR = StExp

-- | Parse a generic LHS/RHS with sigils.
expHsGeneric :: Parser StExp
expHsGeneric = do
    -- sepBy1 guarantees non-empty list
    (head:rest) <- (    StringNotCode . T.unpack <$> ident
                    <|> CodeNotString . T.unpack <$> ("$" *> haskell))
                   `Ap.sepBy1` Ap.string ":"
    let isCodeNotString x = case x of
            CodeNotString _ -> True
            _ -> False
    -- Don't use custom patterns where generic ones work
    if (isCodeNotString head || any isCodeNotString rest)
        then return $ ExpHsGeneric head rest
        else fail "no variable component"

-- | Custom component for statement LHS. This parses the LHS variable sigils.
e_lhs :: Parser StExpL
e_lhs = expHsGeneric
    <|> ExpHsDirect  . T.unpack <$> ("%" *> haskell)
    <|> ExpHsInt     . T.unpack <$> ("!" *> haskell)

-- | Custom component for statement RHS. This parses the RHS variable sigils.
e_rhs :: Parser StExpR
e_rhs = expHsGeneric
    <|> ExpHsDirect   . T.unpack <$> ("%" *> haskell)
    <|> ExpHsString   . T.unpack <$> ("?" *> haskell)
    <|> ExpHsCompound . T.unpack <$> ("@" *> haskell)

-- | Haskell expression: either a variable, or a more complex expression in
-- parentheses. Permitted syntax is whatever is supported by
-- "Language.Haskell.Meta" in expression context. Notably, this does /not/
-- include TH splices or quasiquotation.
haskell :: Parser Text
haskell
    = ident
  <|> (Ap.string "(" *> haskell_body <* Ap.string ")")

-- | Body of a Haskell expression. This stage interprets the expression as pure
-- text; it is parsed as Haskell later.
haskell_body :: Parser Text
haskell_body = do
    next <- Ap.peekChar
    case next of
        Nothing  -> fail "eof inside antiquote"
        Just ')' -> return ""
        Just '(' -> (<>) <$> haskell_parens <*> haskell_body
        _        -> (<>) <$> (T.singleton <$> Ap.anyChar) <*> haskell_body

-- | A parenthesized Haskell expression. This stage interprets the expression
-- as pure text; it is parsed as Haskell later.
haskell_parens :: Parser Text
haskell_parens = T.concat <$> sequence
    [Ap.string "("
    ,haskell_body
    ,Ap.string ")"
    ]

-- | Expression component of the quasiquoter.
pdx_e :: String -> Q Exp
pdx_e s = case Ap.parseOnly (Ap.skipSpace *> statement e_lhs e_rhs <* Ap.skipSpace) (fromString s) of
                Left _ -> error "invalid statement (expression)"
                Right e -> prostmt2stmt e

-- | Convert a statement into the equivalent expression.
prostmt2stmt :: Statement StExpL StExpR -> Q Exp
prostmt2stmt (StatementBare _) = error "bare statement passed to prostmt2stmt"
prostmt2stmt (Statement lhs op rhs) = [| Statement $(lhs2exp lhs) $(op2exp op) $(rhs2exp rhs) |]

-- | Construct an expression splice from a Haskell snippet. If parsing fails,
-- abort.
mkHsVal :: String -- ^ Haskell expression concrete syntax
        -> Q Exp
mkHsVal s = case parseExp s of
    Right expr -> return expr
    Left err -> fail ("couldn't parse haskell: " ++ err)

-- Helper, used in several places
rhsgen2splice :: StringOrCode -> Q Exp
rhsgen2splice = \case
    StringNotCode t -> stringE t
    CodeNotString t -> mkHsVal t

-- | Turn an LHS AST expression into a splice. If it started with a sigil,
-- interpret the Haskell expression following it.
lhs2exp :: Lhs StExpL -> Q Exp
lhs2exp (CustomLhs (ExpHsGeneric (CodeNotString s) mt))
    = [| GenericLhs $(mkHsVal s) $(listE (map rhsgen2splice mt)) |]
lhs2exp (CustomLhs (ExpHsGeneric (StringNotCode s) mt))
    = [| GenericLhs $(stringE s) $(listE (map rhsgen2splice mt)) |]
lhs2exp (CustomLhs (ExpHsDirect s)) = varE (mkName s)
lhs2exp (CustomLhs _) = fail "BUG: invalid lhs"
lhs2exp other = [| other |]

-- | Splice an operator from its AST.
op2exp :: Operator -> Q Exp
op2exp OpEq = conE 'OpEq
op2exp OpLT = conE 'OpLT
op2exp OpGT = conE 'OpGT

-- | Turn an RHS AST expression into a splice. If it started with a sigil,
-- interpret the Haskell expression following it.
rhs2exp :: Rhs StExpL StExpR -> Q Exp
rhs2exp (CustomRhs (ExpHsDirect s)) = case parseExp s of
    Right expr -> return expr
    Left err -> fail ("couldn't parse haskell: " ++ err)
rhs2exp (CustomRhs (ExpHsGeneric (CodeNotString s) mt))
    = [| GenericRhs $(mkHsVal s) $(listE (map rhsgen2splice mt)) |]
rhs2exp (CustomRhs (ExpHsGeneric (StringNotCode s) mt))
    = [| GenericRhs $(stringE s) $(listE (map rhsgen2splice mt)) |]
rhs2exp (CustomRhs (ExpHsString s))   = [| StringRhs   $(mkHsVal s) |]
rhs2exp (CustomRhs (ExpHsInt s))      = [| IntRhs      $(mkHsVal s) |]
rhs2exp (CustomRhs (ExpHsCompound s)) = [| CompoundRhs $(mkHsVal s) |]
rhs2exp other = [| other |]

--------------
-- Patterns --
--------------

-- | Pattern component of the quasiquoter.
pdx_p :: String -> Q Pat
pdx_p s = case Ap.parseOnly (Ap.skipSpace *> statement p_lhs p_rhs <* Ap.skipSpace) (fromString s) of
                Left _ -> error "invalid statement (pattern)"
                Right propat -> propat2pat propat

-- | Pattern AST.
data StPat
    = PatHs String          -- ^ %foo => (pattern) foo
    | PatStringlike String  -- ^ ?foo => (textRhs -> foo) (Only makes sense on RHS)
    | PatStringOrNum String -- ^ ?!foo => (floatOrTextRhs -> foo) (Only makes sense on RHS)
    | PatSomeGeneric StringOrCode [StringOrCode]
        -- ^ * $foo => PatSomeGeneric (CodeNotString foo) []
        --   * foo:bar => handled elsewhere
        --   * $foo:bar => PatSomeGeneric (CodeNotString foo) (StringNotCode bar)
        --   * foo:$bar => PatSomeGeneric (StringNotCode foo) (CodeNotString bar)
        --   * $foo:bar => PatSomeGeneric (CodeNotString foo) (CodeNotString bar)
    | PatSomeInt String     -- ^ !foo => (floatRhs -> foo) (Only makes sense on RHS)
    | PatCompound String    -- ^ @foo => CompoundRhs foo (Only makes sense on RHS)
    deriving (Show, Eq, Ord)
type StPatL = StPat
type StPatR = StPat

atoms_p :: Parser StPat
atoms_p = do
    -- sepBy1 guarantees non-empty list
    (head:rest) <- (    StringNotCode . T.unpack <$> ident
                    <|> CodeNotString . T.unpack <$> ("$" *> haskell))
                   `Ap.sepBy1` Ap.string ":"
    let isCodeNotString x = case x of
            CodeNotString _ -> True
            _ -> False
    -- Don't use custom patterns where generic ones work
    if (isCodeNotString head || any isCodeNotString rest)
        then return $ PatSomeGeneric head rest
        else fail "no variable component"

-- | Custom component for statement LHS. This parses the LHS variable sigils.
p_lhs :: Parser StPatL
p_lhs = (PatHs          . T.unpack) <$> ("%"  *> haskell) -- %foo => type inferred
    <|> atoms_p -- foo:$bar:baz:$(quux quuux) - any combo of literal or variable
    -- If no sigil, fall back to normal handling.
    <|> (PatSomeInt     . T.unpack) <$> ("!" *> haskell)

-- | Custom component for statement RHS. This parses the RHS variable sigils.
p_rhs :: Parser StPatR
p_rhs = (PatStringOrNum . T.unpack) <$> ("?!" *> haskell)
    <|> (PatStringlike  . T.unpack) <$> ("?" *> haskell)
    <|> (PatCompound    . T.unpack) <$> ("@" *> haskell)
    <|> p_lhs

-- | Convert a statement into the equivalent pattern.
propat2pat :: Statement StPatL StPatR -> Q Pat
propat2pat (StatementBare _) = error "bare statement passed to propat2pat"
propat2pat (Statement lhs op rhs) = [p| Statement $(lhs2pat lhs) $(op2pat op) $(rhs2pat rhs) |]

-- | Turn an LHS AST pattern into a splice. If it started with a sigil,
-- interpret the Haskell pattern following it.
lhs2pat :: Lhs StPatL -> Q Pat
lhs2pat lhs = case lhs of
    -- %foo => any lhs, stored in a variable foo
    CustomLhs (PatHs hpat) -> case parsePat hpat of
        Right pat' -> return pat'
        Left err -> fail ("couldn't parse pattern: " ++ err)
    -- $foo => generic lhs, name stored in a variable foo
    --  e.g. $color = yes => Statement (GenericLhs (CodeNotString color) Nothing)
    --                                 (GenericRhs (StringNotCode "yes") Nothing)
    CustomLhs (PatSomeGeneric s mt) ->
        let tags = flip map mt $ \case
                StringNotCode t -> [p| $(litP (stringL t)) |]
                CodeNotString t -> [p| $(case parsePat t of
                    Right t' -> return t'
                    Left err -> fail ("couldn't parse generic pattern: " ++ err)) |]
        in case s of
            CodeNotString c ->
                if null mt && c == "_"
                    then [p| _ |] -- wildcard, not a variable named "_"!
                    else [p| GenericLhs $(varP (mkName c)) $(listP tags) |]
            StringNotCode c ->
                [p| GenericLhs $(litP (stringL c)) $(listP tags) |]
    -- not supported in LHS
    CustomLhs (PatSomeInt n) -> [p| IntLhs $(varP (mkName n)) |]
    CustomLhs (PatCompound _) -> error "compound pattern not supported on LHS"
    CustomLhs (PatStringlike _) -> error "stringlike pattern not supported on LHS"
    CustomLhs (PatStringOrNum _) -> error "string-or-num pattern not supported on LHS"
    -- non-custom
    AtLhs label -> error "statement starting with @ not supported on LHS"
    IntLhs _ -> error "int pattern not supported on LHS"
    GenericLhs gs gts ->
        [p| GenericLhs $(litP (stringL (T.unpack gs)))
                       $(listP (map (litP . stringL . T.unpack) gts)) |]

-- | Splice an operator from its AST.
op2pat :: Operator -> Q Pat
op2pat OpEq = [p| OpEq |]
op2pat OpLT = [p| OpLT |]
op2pat OpGT = [p| OpGT |]

-- | Turn an RHS AST pattern into a splice. If it started with a sigil,
-- interpret the Haskell pattern following it.
rhs2pat :: Rhs StPatL StPatR -> Q Pat
rhs2pat rhs = case rhs of
    -- @foo => any rhs, stored in a variable foo
    CustomRhs (PatHs varid)
        | varid == "_" -> wildP
        | otherwise -> varP (mkName varid)
    -- ?foo => generic rhs, name stored in a variable foo
    --  e.g. color = ?color => Statement (GenericRhs "color") (GenericRhs color)
    CustomRhs (PatStringlike patS) -> case parsePat patS of
        Right pat' -> viewP [| textRhs |] [p| Just $(return pat') |]
        Left  err  -> fail ("couldn't parse string pattern: " ++ err)
    CustomRhs (PatStringOrNum patS) -> case parsePat patS of
        Right pat' -> viewP [| floatOrTextRhs |] (return pat')
        Left  err  -> fail ("couldn't parse string pattern: " ++ err)
    CustomRhs (PatSomeGeneric s mt) ->
        let tags = flip map mt $ \case
                StringNotCode t -> [p| $(litP (stringL t)) |]
                CodeNotString t -> [p| $(case parsePat t of
                    Right t' -> return t'
                    Left err -> fail ("couldn't parse generic pattern: " ++ err)) |]
        in case s of
            CodeNotString patS -> case parsePat patS of
                Right pat' -> [p| GenericRhs $(return pat') $(listP tags) |]
                Left err -> fail ("couldn't parse generic pattern: " ++ err)
            StringNotCode s -> [p| GenericRhs $(litP (stringL s)) $(listP tags) |]
    CustomRhs (PatSomeInt patS) -> case parsePat patS of
        Right pat' -> viewP [| floatRhs |] [p| Just $(return pat') |]
        Left  err  -> fail ("couldn't parse integer pattern: " ++ err)
    CustomRhs (PatCompound patS) -> case parsePat patS of
        Right pat' -> [p| CompoundRhs $(return pat') |]
        Left  err  -> fail ("couldn't parse compound pattern: " ++ err)
    GenericRhs gen tags -> [p| GenericRhs $(litP  (stringL (T.unpack gen)))
                                          $(listP (map (litP . stringL . T.unpack) tags)) |]
    StringRhs  str     -> [p| StringRhs   $(litP  (stringL (T.unpack str))) |]
    IntRhs     int     -> [p| IntRhs      $(litP  (integerL (fromIntegral int))) |]
    CompoundRhs stmts  -> [p| CompoundRhs $(listP (map propat2pat stmts)) |]
    FloatRhs   flt     -> [p| FloatRhs    $(litP  (doublePrimL (toRational flt))) |]
    DateRhs    _       -> error "date literal not yet supported in patterns, sorry!"

---------------------
-- The quasiquoter --
---------------------

TL.deriveLiftMany [''StringOrCode, ''Lhs, ''Rhs, ''Operator, ''Date, ''Statement, ''StPat, ''StExp]

-- | The quasiquoter. Support is provided for expression and pattern contexts;
-- type and declaration contexts are not meaningful and therefore not
-- supported.
pdx :: QuasiQuoter
pdx = QuasiQuoter {
            quoteExp  = pdx_e
        ,   quotePat  = pdx_p
        ,   quoteType = pdx_t
        ,   quoteDec  = pdx_d
        }

-- | Type component for the quasiquoter. Since this is not meaningful, it just
-- throws an error.
pdx_t :: String -> Q Type
pdx_t = error "quasiquoting statements in type context not supported"

-- | Declaration component for the quasiquoter. Since this is not meaningful,
-- it just throws an error.
pdx_d :: String -> Q [Dec]
pdx_d = error "quasiquoting statements in declaration context not supported"
