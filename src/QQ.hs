{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QQ (
        pdx     -- :: QuasiQuoter
    ) where

{-

    The scr quasiquoter allows literal scripts to substitute for the script
    data structures. To interpolate or bind variables, it uses the following
    syntax. (foo is either a variable name or a parenthesised Haskell
    expression.)

    In an expression:
        $foo => foo is a bare_word (Also makes sense on LHS)
            e.g. if foo = "bar", then
                [pdx|foo = $bar|] => foo = bar
        %foo => foo is the relevant script type
            e.g. if foo = GenericRhs "foo", then
                [pdx|%foo|] => type error
                [pdx|bar = %foo|] => bar = foo
                [pdx|foo = %(case foo of GenericRhs s -> StringRhs (s <> "bar"))|]
                    => foo = "foobar"
        ?foo => "<contents of foo>" (foo :: Text)
            e.g. if foo = "foo", [pdx|foo = ?foo|] => foo = "foo"
        !foo => foo is an Int
            e.g. if foo = 23, [pdx|foo = !foo|] => foo = 23
        @foo => foo is a compound RHS
            e.g. if foo = [pdx1, scr2],
                [pdx|foo = @foo|] => foo = { %scr1 %scr2 }

    In a pattern:
        on the LHS or RHS:
            @foo => foo is bound as the relevant script type
                e.g. [pdx|@foo|] => foo :: GenericScript
                     [pdx|foo = @foo|] => foo :: GenericRhs
            $foo => foo :: Text, matches a bare_word
            ?foo => foo :: Text, matches a "string" /or/ a bare_word
                                 (goes through textRhs)
        on the RHS only:
            !foo => foo :: Int, matches a number (goes through floatRhs)
            [foo] => foo :: GenericScript, matches a compound

-}

import Control.Applicative
import Data.Monoid

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Ap
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

import Language.Haskell.TH
import qualified Language.Haskell.TH.Lift as TL
import Language.Haskell.TH.Quote
import Instances.TH.Lift ()
--import Language.Haskell.Exts.QQ
import Language.Haskell.Meta

import Abstract

-----------------
-- Expressions --
-----------------

data StExp
    = ExpHsDirect String    -- %foo => foo :: GenericLhs etc.
    | ExpHsGeneric String   -- $foo => Generic[LR]hs foo
    | ExpHsString String    -- ?foo => StringRhs foo
    | ExpHsInt String       -- !foo => IntRhs foo
    | ExpHsCompound String  -- @foo => CompoundRhs foo
    deriving (Show, Eq, Ord)
    -- TODO: dates
type StExpL = StExp
type StExpR = StExp

e_lhs :: Parser StExpL
e_lhs = ExpHsGeneric . T.unpack <$> (Ap.string "$" *> haskell)
    <|> ExpHsDirect  . T.unpack <$> (Ap.string "%" *> haskell)

e_rhs :: Parser StExpR
e_rhs = ExpHsDirect   . T.unpack <$> (Ap.string "%" *> haskell)
    <|> ExpHsGeneric  . T.unpack <$> (Ap.string "$" *> haskell)
    <|> ExpHsString   . T.unpack <$> (Ap.string "?" *> haskell)
    <|> ExpHsInt      . T.unpack <$> (Ap.string "!" *> haskell)
    <|> ExpHsCompound . T.unpack <$> (Ap.string "@" *> haskell)

haskell :: Parser Text
haskell
    = ident
  <|> (Ap.string "(" *> haskell_body <* Ap.string ")")

haskell_body :: Parser Text
haskell_body = do
    next <- Ap.peekChar
    case next of
        Nothing  -> fail "eof inside antiquote"
        Just ')' -> return ""
        Just '(' -> (<>) <$> haskell_parens <*> haskell_body
        _        -> (<>) <$> (T.singleton <$> Ap.anyChar) <*> haskell_body

haskell_parens :: Parser Text
haskell_parens = T.concat <$> sequence
    [Ap.string "("
    ,haskell_body
    ,Ap.string ")"
    ]

pdx_e :: String -> Q Exp
pdx_e s = case Ap.parseOnly (Ap.skipSpace *> statement e_lhs e_rhs <* Ap.skipSpace) (fromString s) of
                Left _ -> error "invalid statement (expression)"
                Right e -> prostmt2stmt e

prostmt2stmt :: Statement StExpL StExpR -> Q Exp
prostmt2stmt (StatementBare _) = error "bare statement passed to prostmt2stmt"
prostmt2stmt (Statement lhs op rhs) = [| Statement $(lhs2exp lhs) $(op2exp op) $(rhs2exp rhs) |]

mkHsVal :: Name -> String -> Q Exp
mkHsVal con s = case parseExp s of
    Right expr -> return $ AppE (ConE con) expr
    Left err -> fail ("couldn't parse haskell: " ++ err)

lhs2exp :: Lhs StExpL -> Q Exp
lhs2exp (CustomLhs (ExpHsGeneric s)) = mkHsVal 'GenericLhs s
lhs2exp (CustomLhs (ExpHsDirect s)) = varE (mkName s)
lhs2exp (CustomLhs _) = fail "BUG: invalid lhs"
lhs2exp other = [| other |]

op2exp :: Operator -> Q Exp
op2exp OpEq = conE 'OpEq
op2exp OpLT = conE 'OpLT
op2exp OpGT = conE 'OpGT

rhs2exp :: Rhs StExpL StExpR -> Q Exp
rhs2exp (CustomRhs (ExpHsDirect s)) = case parseExp s of
    Right expr -> return expr
    Left err -> fail ("couldn't parse haskell: " ++ err)
rhs2exp (CustomRhs (ExpHsGeneric s))  = mkHsVal 'GenericRhs s
rhs2exp (CustomRhs (ExpHsString s))   = mkHsVal 'StringRhs s
rhs2exp (CustomRhs (ExpHsInt s))      = mkHsVal 'IntRhs s
rhs2exp (CustomRhs (ExpHsCompound s)) = mkHsVal 'CompoundRhs s
rhs2exp other = [| other |]

--------------
-- Patterns --
--------------

pdx_p :: String -> Q Pat
pdx_p s = case Ap.parseOnly (Ap.skipSpace *> statement p_lhs p_rhs <* Ap.skipSpace) (fromString s) of
                Left _ -> error "invalid statement (pattern)"
                Right propat -> propat2pat propat

data StPat
    = PatHs String          -- %foo => (pattern) foo
    | PatStringlike String  -- ?foo => (textRhs -> foo) (Only makes sense on RHS)
    | PatStringOrNum String -- ?!foo => (floatOrTextRhs -> foo) (Only makes sense on RHS)
    | PatSomeGeneric String -- $foo => GenericLhs foo
    | PatSomeInt String     -- !foo => (floatRhs -> foo) (Only makes sense on RHS)
    | PatCompound String    -- @foo => CompoundRhs
    deriving (Show, Eq, Ord)
type StPatL = StPat
type StPatR = StPat

p_lhs :: Parser StPatL
p_lhs = (PatHs          . T.unpack) <$> (Ap.string "%"  *> haskell)
    <|> (PatSomeGeneric . T.unpack) <$> (Ap.string "$"  *> haskell)
p_rhs :: Parser StPatR
p_rhs = (PatSomeInt     . T.unpack) <$> (Ap.string "!" *> haskell)
    <|> (PatStringOrNum . T.unpack) <$> (Ap.string "?!" *> haskell)
    <|> (PatStringlike  . T.unpack) <$> (Ap.string "?" *> haskell)
    <|> (PatCompound    . T.unpack) <$> (Ap.string "@" *> haskell)
    <|> p_lhs

propat2pat :: Statement StPatL StPatR -> Q Pat
propat2pat (StatementBare _) = error "bare statement passed to propat2pat"
propat2pat (Statement lhs op rhs) = [p| Statement $(lhs2pat lhs) $(op2pat op) $(rhs2pat rhs) |]
{-
propat2pat (Statement lhs op rhs) = do
    left <- lhs2pat lhs
    opr <- op2pat op
    right <- rhs2pat rhs
    return $ ConP 'Statement [left, opr, right]
-}

lhs2pat :: Lhs StPatL -> Q Pat
lhs2pat lhs = case lhs of
    -- @foo => any lhs, stored in a variable foo
    CustomLhs (PatHs hpat) -> case parsePat hpat of
        Right pat' -> return pat'
        Left err -> fail ("couldn't parse pattern: " ++ err)
    -- ?foo => generic lhs, name stored in a variable foo
    --  e.g. ?color = yes => Statement (GenericLhs color) (GenericRhs "yes")
    CustomLhs (PatSomeGeneric gen) ->
        if gen == "_"
        then wildP
        else conP 'GenericLhs [varP (mkName gen)]
    -- not supported in LHS
    CustomLhs (PatSomeInt _) -> error "int pattern not supported on LHS"
    CustomLhs (PatCompound _) -> error "compound pattern not supported on LHS"
    CustomLhs (PatStringlike _) -> error "stringlike pattern not supported on LHS"
    CustomLhs (PatStringOrNum _) -> error "string-or-num pattern not supported on LHS"
    IntLhs _ -> error "int pattern not supported on LHS"
    GenericLhs gen -> conP 'GenericLhs [litP (stringL (T.unpack gen))]

op2pat :: Operator -> Q Pat
op2pat OpEq = [p| OpEq |]
op2pat OpLT = [p| OpLT |]
op2pat OpGT = [p| OpGT |]

rhs2pat :: Rhs StPatL StPatR -> Q Pat
rhs2pat rhs = case rhs of
    -- @foo => any rhs, stored in a variable foo
    CustomRhs (PatHs varid)
        | varid == "_" -> wildP
        | otherwise -> varP (mkName varid)
    -- ?foo => generic rhs, name stored in a variable foo
    --  e.g. color = ?color => Statement (GenericLhs "color") (GenericRhs color)
    CustomRhs (PatStringlike patS) -> case parsePat patS of
        Right pat' -> viewP [| textRhs |] [p| Just $(return pat') |]
        Left  err  -> fail ("couldn't parse string pat'tern: " ++ err)
    CustomRhs (PatStringOrNum patS) -> case parsePat patS of
        Right pat' -> viewP [| floatOrTextRhs |] (return pat')
        Left  err  -> fail ("couldn't parse string pat'tern: " ++ err)
    CustomRhs (PatSomeGeneric patS) -> case parsePat patS of
        Right pat' -> [p| GenericRhs $(return pat') |]
        Left  err  -> fail ("couldn't parse generic pat'tern: " ++ err)
    CustomRhs (PatSomeInt patS) -> case parsePat patS of
        Right pat' -> viewP [| floatRhs |] [p| Just $(return pat') |]
        Left  err  -> fail ("couldn't parse integer pat'tern: " ++ err)
    CustomRhs (PatCompound patS) -> case parsePat patS of
        Right pat' -> [p| CompoundRhs $(return pat') |]
        Left  err  -> fail ("couldn't parse compound pat'tern: " ++ err)
    GenericRhs gen    -> [p| GenericRhs  $(litP  (stringL (T.unpack gen))) |]
    StringRhs  str    -> [p| StringRhs   $(litP  (stringL (T.unpack str))) |]
    IntRhs     int    -> [p| IntRhs      $(litP  (integerL (fromIntegral int))) |]
    CompoundRhs stmts -> [p| CompoundRhs $(listP (map propat2pat stmts)) |]
    FloatRhs   flt    -> [p| FloatRhs    $(litP  (doublePrimL (toRational flt))) |]
    DateRhs    _      -> error "date literal not yet supported in patterns, sorry!"

---------------------
-- The quasiquoter --
---------------------

TL.deriveLiftMany [''Lhs, ''Rhs, ''Operator, ''Date, ''Statement, ''StPat, ''StExp]

pdx :: QuasiQuoter
pdx = QuasiQuoter {
            quoteExp  = pdx_e
        ,   quotePat  = pdx_p
        ,   quoteType = pdx_t
        ,   quoteDec  = pdx_d
        }

pdx_t :: String -> Q Type
pdx_t = error "quasiquoting statements in type context not supported"

pdx_d :: String -> Q [Dec]
pdx_d = error "quasiquoting statements in declaration context not supported"
