{-# OPTIONS_GHC -fno-warn-orphans #-}
module MessageTools (
    -- * Numbers
    -- ** Plain formatting
        plainNum, roundNum, plainNumSign
    ,   roundNumNoSpace
    ,   plainPc, roundPc, plainPcSign
    -- ** Coloured formatting
    -- | These functions take an additional 'Bool' argument that specifies
    -- whether a positive quantity is good (@True@) or bad (@False@). It
    -- formats the number using a @{{red}}@ or @{{green}}@ template
    -- accordingly. Don't use these for numbers that may be either good or bad
    -- depending on context, e.g. karma.
    ,   colourNum, colourPc
    ,   colourNumSign, colourPcSign
    -- ** Reduced numbers
    -- | Several quantities range from 0 to 100 in game, but are expressed in
    -- script as a number between 0 and 1. This includes religous quantities
    -- (e.g. patriarch authority), government strength (e.g. republican
    -- tradition), economic quantities (e.g. mercantilism), etc. To present
    -- these, pass your chosen presentation function to 'reducedNum'.
    ,   reducedNum
    -- * Plural
    ,   plural
    -- * Gain/lose
    -- | These functions hardcode their message fragments. They will have to
    -- be duplicated for languages other than English.
    ,   gainOrLose, gainsOrLoses
    -- * Wiki markup
    ,   template, templateDoc
    -- * If-then-else
    ,   ifThenElse, ifThenElseT
    -- * General text formatting
    ,   iquotes, bold, boldText
    -- * The 'ppNumSep' number formatting method
    ,   PPSep (..)
    ,   module Text.Shakespeare.I18N
    ,   module Text.PrettyPrint.Leijen.Text
    ) where

import Data.List (unfoldr, intersperse)
import Data.Monoid ((<>))

import Numeric (floatToDigits)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.Shakespeare.I18N (ToMessage (..))

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Doc

instance ToMessage Doc where
    toMessage = Doc.doc2text

----------------------
-- Printing numbers --
----------------------

-- | Pretty-print a number, adding &amp;#8239; (U+202F NARROW NO-BREAK SPACE)
-- at every power of 1000.
class Num a => PPSep a where
    ppNumSep :: a -> Doc

-- | Split a list into groups of 3 elements.
group3 :: [a] -> [[a]]
group3 = unfoldr (\cs -> if null cs then Nothing else Just (splitAt 3 cs))

instance PPSep Integer where
    ppNumSep n = Doc.strictText . T.pack $
            (if n < 0 then "−" else "") <> ppNumSep' True (show (abs n))

-- Split into groups of 3 and intersperse the groups with narrow no-break
-- spaces.
-- If first arg is True, start grouping at the end (e.g. for integers).
ppNumSep' :: Bool -> String -> String
ppNumSep' isint
    = mconcat
        . (if isint then reverse else id)
        . intersperse "&#8239;"
        . (if isint then map reverse else id)
        . group3 
        . (if isint then reverse else id)

instance PPSep Int where
    ppNumSep = ppNumSep . toInteger

instance PPSep Double where
    ppNumSep n
        = let absn = abs n
              (digits, expn) = floatToDigits 10 absn
              (_, fracDigits') = splitAt expn digits
              -- fracDigits' is [] if exp is a nonzero whole number
              fracDigits = if fracDigits' == [0] then [] else fracDigits'
          in (if n < 0 then "−" else "")
                <> PP.text (TL.pack . ppNumSep' True $ show (truncate absn :: Int))
                <> (if null fracDigits
                    then ""
                    else "."
                         <> PP.text (TL.pack . ppNumSep' False $
                             replicate (negate expn) '0' -- zeroes after decimal
                             ++ concatMap show fracDigits))

-- | Format a number as is, except add thousands separators.
plainNum :: Double -> Doc
plainNum = ppNum False False False False

-- | Format a number as is, except add thousands separators and a sign.
plainNumSign :: Double -> Doc
plainNumSign = ppNum False False False True

-- | Format a number as a percentage. Add thousands separators.
plainPc :: Double -> Doc
plainPc = ppNum False True False False

-- | Format a number as is, except add thousands separators, a sign and a percent sign.
plainPcSign :: Double -> Doc
plainPcSign = ppNum False True False True

-- | Front end to 'ppNum' for uncoloured numbers.
roundNum' :: Bool -- ^ Whether to treat this number as a percentage
          -> Bool -- ^ Whether to add a + if this number is positive
          -> Double -> Doc
roundNum' is_pc pos_plus n =
    let rounded :: Int
        rounded = round n
    in ppNum False is_pc True pos_plus rounded

-- | Format a number, but make sure it's an integer by rounding it off. Add
-- thousands separators.
roundNum :: Double -> Doc
roundNum = roundNum' False False

-- | Format a number, but make sure it's an integer by rounding it off. Don't
-- add thousands separators.
roundNumNoSpace :: (RealFrac n, PPSep n) => n -> Text
roundNumNoSpace n = Doc.doc2text $ PP.integer (round n :: Integer)

-- | Format a number as a percentage, but make sure it's an integer by rounding
-- it off. Add thousands separators.
roundPc :: Double -> Doc
roundPc = roundNum' True False

-- | Format a number in an appropriate colour with thousands separators.
colourNum :: Bool -> Double -> Doc
colourNum good = ppNum True False good False

-- | Format a number as a percentage, in an appropriate colour, with thousands separators.
colourPc :: Bool -> Double -> Doc
colourPc good = ppNum True True good False

-- | Format a number in an appropriate colour with thousands separators, adding
-- a @+@ at the start if positive.
colourNumSign :: Bool -> Double -> Doc
colourNumSign good = ppNum True False good True

-- | Format a number as a percentage in an appropriate colour with thousands
-- separators, adding a @+@ at the start if positive.
colourPcSign :: Bool -> Double -> Doc
colourPcSign good = ppNum True True good True

-- | Format a number using the given function, but multiply it by 100 first.
reducedNum :: PPSep n => (n -> Doc) -> n -> Doc
reducedNum p n = p (n * 100)

-- | Pretty-print a number.
ppNum :: (Ord n, PPSep n) => Bool -- ^ Whether to apply a colour template (red
                                  --   for bad, green for good).
                          -> Bool -- ^ Whether to treat this number as a
                                  --   percentage, i.e. add a percentage sign.
                          -> Bool -- ^ Whether positive is good and negative is
                                  --   bad, or vice versa. Ignored if the first
                                  --   argument is False.
                          -> Bool -- ^ Whether to add a + to positive numbers,
                                  --   or strip the - from negative ones.
                          -> n -> Doc
ppNum colour is_pc pos pos_plus n =
    let n_pp'd = (if pos_plus then Doc.pp_signed else Doc.pp_nosigned)
                 ppNumSep n <> if is_pc then "%" else ""
    in (if not colour then id else
        case (if pos then n else negate n) `compare` 0 of
            LT -> template "red" . (:[]) . Doc.doc2text
            EQ -> bold
            GT -> template "green" . (:[]) . Doc.doc2text)
        n_pp'd

-- | If the numeric argument is singular, return the second argument; otherwise
-- return the third argument.
--
-- This is for cases where the form of a word depends on whether the number is
-- 1 or something else. For example, instead of @Has at least #{n} province(s)@:
--
-- * Has at least 1 province(s)
-- * Has at least 2 province(s)
--
-- we can say @Has at least #{n} #{plural n "province" "provinces"}@, which gives
-- the following, prettier output:
--
-- * Has at least 1 province
-- * Has at least 2 provinces
plural :: (Eq n, Num n) => n -> Text -> Text -> Text
plural n sing plur | n == 1    = sing
                   | otherwise = plur

-- | Say "Gain" or "Lose" (with that capitalisation) depending on whether the
-- numeric argument is positive or negative (respectively).
gainOrLose :: (Ord n, Num n) => n -> Text
gainOrLose n | n < 0     = "Lose"
             | otherwise = "Gain"

-- | Say "gains" or "loses" (with that capitalisation) depending on whether the
-- numeric argument is positive or negative (respectively).
gainsOrLoses :: (Ord n, Num n) => n -> Text
gainsOrLoses n | n < 0     = "loses"
               | otherwise = "gains"

-----------------
---- Wiki text --
-----------------

-- | Template with arguments. @template "foo" ["bar","baz"]@ produces
-- @{{foo|bar|baz}}@.
template :: Text -> [Text] -> Doc
template name args = templateDoc (Doc.strictText name) (map Doc.strictText args)

-- | Doc version of 'template'.
templateDoc :: Doc -> [Doc] -> Doc
templateDoc name args = PP.hcat $
    "{{"
    : (intersperse "|" (name:args)
      ++ ["}}"])

-- | Set text in italics, and wrap in quotation marks. Use this for short
-- localized strings such as modifier and event names.
iquotes :: Text -> Doc
iquotes = PP.enclose "''“" "”''" . Doc.strictText

---- Set doc in italics.
--italic :: Doc -> Doc
--italic = enclose "''" "''"

-- | Set doc in boldface. Take care: if the text passed to this begins or ends
-- with an apostrophe, you may get incorrect results. Mixing with italics,
-- however, does work.
bold :: Doc -> Doc
bold = PP.enclose "'''" "'''"

-- | Set text in boldface. Take care: if the text passed to this begins or ends
-- with an apostrophe, you may get incorrect results. Mixing with italics,
-- however, does work.
boldText :: Text -> Text
boldText = Doc.doc2text . bold . Doc.strictText

-- | Produce output based on a boolean (i.e. if-then-else). Needed because the
-- i18n templates don't understand this syntax, but instead interpret these
-- three keywords as identifiers.
--
-- You don't need to use this in the non-TH version of "Messages". Just use
-- plain old if-then-else.
ifThenElse :: Bool -> a -> a -> a
ifThenElse yn yes no = if yn then yes else no

-- | As 'ifThenElse', but specialized to 'Text'. This is needed because, in the
-- presence of the OverloadedStrings extension, type inference doesn't know
-- what specific string type you mean when you use a string literal.
ifThenElseT :: Bool -> Text -> Text -> Text
ifThenElseT = ifThenElse
