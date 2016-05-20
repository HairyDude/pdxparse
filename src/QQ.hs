{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module QQ (
        stmt    -- :: QuasiQuoter
    ) where

{-

    The stmt quasiquoter allows literal scripts to substitute for the script
    data structures. To interpolate or bind variables, it uses the following
    syntax. (foo is either a variable name or a parenthesised Haskell
    expression.)

    In an expression:
        $foo => foo is a bare_word (Also makes sense on LHS)
            e.g. if foo = "bar", then
                [stmt|foo = $bar|] => foo = bar
        %foo => foo is the relevant script type
            e.g. if foo = GenericRhs "foo", then
                [stmt|%foo|] => type error
                [stmt|bar = %foo|] => bar = foo
                [stmt|foo = %(case foo of GenericRhs s -> StringRhs (s <> "bar"))|]
                    => foo = "foobar"
        ?foo => "<contents of foo>" (foo :: Text)
            e.g. if foo = "foo", [stmt|foo = ?foo|] => foo = "foo"
        !foo => foo is an Int
            e.g. if foo = 23, [stmt|foo = !foo|] => foo = 23
        @foo => foo is a compound RHS
            e.g. if foo = [stmt1, stmt2],
                [stmt|foo = @foo|] => foo = { %stmt1 %stmt2 }

    In a pattern:
        on the LHS or RHS:
            @foo => foo is bound as the relevant script type
                e.g. [stmt|@foo|] => foo :: GenericScript
                     [stmt|foo = @foo|] => foo :: GenericRhs
            $foo => foo :: Text, matches a bare_word
        on the RHS only:
            !foo => foo :: Int, matches an integer
            [foo] => foo :: GenericScript, matches a compound

    Note that it's not possible to make a simple pattern that matches either a
    string or a bare_word. For that case, you need to use textRhs in a pattern
    guard:
        case statement of
            [stmt|foo = @rfoo|] | Just foo <- textRhs rfoo -> ...

-}

import Control.Applicative
import Control.Monad
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
import Language.Haskell.Exts.QQ
import Language.Haskell.Meta

import Abstract

-----------------
-- Expressions --
-----------------

data StExp
    = ExpHsDirect String
    | ExpHsGeneric String
    | ExpHsString String
    | ExpHsInt String
    | ExpHsCompound String
    deriving (Show, Eq, Ord)
    -- TODO: dates
type StExpL = StExp
type StExpR = StExp

e_lhs :: Parser StExpL
e_lhs = ExpHsGeneric . T.unpack <$> (Ap.string "$" *> haskell)

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

stmt_e :: String -> Q Exp
stmt_e s = case Ap.parseOnly (Ap.skipSpace *> statement e_lhs e_rhs <* Ap.skipSpace) (fromString s) of
                Left _ -> error "invalid statement (expression)"
                Right e -> prostmt2stmt e

prostmt2stmt :: Statement StExpL StExpR -> Q Exp
prostmt2stmt (Statement lhs op rhs) = [| Statement $(lhs2exp lhs) $(op2exp op) $(rhs2exp rhs) |]

mkHsVal :: Name -> String -> Q Exp
mkHsVal con s = case parseExp s of
    Right exp -> return $ AppE (ConE con) exp
    Left err -> fail ("couldn't parse haskell: " ++ err)

lhs2exp :: Lhs StExpL -> Q Exp
lhs2exp (CustomLhs (ExpHsGeneric s)) = mkHsVal 'GenericLhs s
lhs2exp (CustomLhs _) = fail "BUG: invalid lhs"
lhs2exp other = [| other |]

op2exp :: Operator -> Q Exp
op2exp OpEq = conE 'OpEq
op2exp OpLT = conE 'OpLT
op2exp OpGT = conE 'OpGT

rhs2exp :: Rhs StExpL StExpR -> Q Exp
rhs2exp (CustomRhs (ExpHsDirect s)) = case parseExp s of
    Right exp -> return exp
    Left err -> fail ("couldn't parse haskell: " ++ err)
rhs2exp (CustomRhs (ExpHsGeneric s))  = mkHsVal 'GenericRhs s
rhs2exp (CustomRhs (ExpHsString s))   = mkHsVal 'StringRhs s
rhs2exp (CustomRhs (ExpHsInt s))      = mkHsVal 'IntRhs s
rhs2exp (CustomRhs (ExpHsCompound s)) = mkHsVal 'CompoundRhs s
rhs2exp other = [| other |]

--------------
-- Patterns --
--------------

stmt_p :: String -> Q Pat
stmt_p s = case Ap.parseOnly (Ap.skipSpace *> statement p_lhs p_rhs <* Ap.skipSpace) (fromString s) of
                Left _ -> error "invalid statement (pattern)"
                Right propat -> return $ propat2pat propat

data StPat
    = PatVar String         -- @foo => (variable) foo
    | PatSomeGeneric String -- ?foo => GenericLhs foo
    | PatSomeInt String     -- !foo => IntRhs foo (Only makes sense on RHS)
    | PatCompound String    -- [foo] -> CompoundRhs
    deriving (Show, Eq, Ord)
type StPatL = StPat
type StPatR = StPat

p_lhs :: Parser StPatL
p_lhs = (PatVar         . T.unpack) <$> (Ap.string "@"  *> ident)
    <|> (PatSomeGeneric . T.unpack) <$> (Ap.string "$"  *> ident)
p_rhs :: Parser StPatR
p_rhs = (PatSomeInt     . T.unpack) <$> (Ap.string "!" *> ident)
    <|> (PatCompound    . T.unpack) <$> (Ap.string "[" *> ident <* Ap.string "]")
    <|> p_lhs

propat2pat :: Statement StPatL StPatR -> Pat
propat2pat (Statement lhs op rhs) = ConP 'Statement [lhs2pat lhs, op2pat op, rhs2pat rhs]

lhs2pat :: Lhs StPatL -> Pat
lhs2pat lhs = case lhs of
    -- @foo => any lhs, stored in a variable foo
    CustomLhs (PatVar id) -> VarP (mkName id)
    -- ?foo => generic lhs, name stored in a variable foo
    --  e.g. ?color = yes => Statement (GenericLhs color) (GenericRhs "yes")
    CustomLhs (PatSomeGeneric id) -> ConP 'GenericLhs [VarP (mkName id)]
    -- not supported in LHS
    CustomLhs (PatSomeInt _) -> error "int pattern not supported on LHS"
    CustomLhs (PatCompound _) -> error "compound pattern not supported on LHS"
    GenericLhs id -> ConP 'GenericLhs [LitP (stringL (T.unpack id))]

op2pat :: Operator -> Pat
op2pat OpEq = ConP 'OpEq []
op2pat OpLT = ConP 'OpLT []
op2pat OpGT = ConP 'OpGT []

rhs2pat :: Rhs StPatL StPatR -> Pat
rhs2pat rhs = case rhs of
    -- @foo => any rhs, stored in a variable foo
    CustomRhs (PatVar id) -> VarP (mkName id)
    -- ?foo => generic rhs, name stored in a variable foo
    --  e.g. color = ?color => Statement (GenericLhs "color") (GenericRhs color)
    CustomRhs (PatSomeGeneric id) -> ConP 'GenericRhs [VarP (mkName id)]
    CustomRhs (PatSomeInt id) -> ConP 'IntRhs [VarP (mkName id)]
    CustomRhs (PatCompound id) -> ConP 'CompoundRhs [VarP (mkName id)]
    GenericRhs id -> ConP 'GenericRhs [LitP (stringL (T.unpack id))]
    StringRhs  str -> ConP 'StringRhs [LitP (stringL (T.unpack str))]
    IntRhs     int  -> ConP 'IntRhs    [LitP (integerL (fromIntegral int))]
    CompoundRhs stmts -> ConP 'CompoundRhs (map propat2pat stmts)

---------------------
-- The quasiquoter --
---------------------

TL.deriveLiftMany [''Lhs, ''Rhs, ''Operator, ''Date, ''Statement, ''StPat, ''StExp]

stmt :: QuasiQuoter
stmt = QuasiQuoter {
            quoteExp = stmt_e
        ,   quotePat = stmt_p
        ,   quoteType = stmt_t
        ,   quoteDec = stmt_d
        }

stmt_t :: String -> Q Type
stmt_t = error "quasiquoting statements in type context not supported"

stmt_d :: String -> Q [Dec]
stmt_d = error "quasiquoting statements in declaration context not supported"
