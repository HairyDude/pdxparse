{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MessageTools (
        plainNum, plainPc
    ,   roundNum, roundPc
    ,   roundNumNoSpace
    ,   colourNum, colourPc
    ,   colourNumSign, colourPcSign
    ,   reducedNum
    ,   plural
    ,   gainOrLose, gainsOrLoses
    ,   template, templateDoc
    ,   ifThenElse, ifThenElseT
    ,   iquotes, bold, boldText
    ,   PPSep (..)
    ,   module Text.Shakespeare.I18N
    ,   module Doc
    ) where

import Data.List
import Data.Monoid

import Numeric (floatToDigits)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.Shakespeare.I18N

import Doc

instance ToMessage Doc where
    toMessage = doc2text

----------------------
-- Printing numbers --
----------------------

-- Pretty-print a number, adding &amp;#8239; (U+202F NARROW NO-BREAK SPACE) at
-- every power of 1000.
class Num a => PPSep a where
    ppNumSep :: a -> Doc

group3 :: [a] -> [[a]]
group3 = unfoldr (\cs -> if null cs then Nothing else Just (splitAt 3 cs))

instance PPSep Integer where
    ppNumSep n = strictText . T.pack $
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
                <> text (TL.pack . ppNumSep' True $ show (truncate absn :: Int))
                <> (if null fracDigits
                    then ""
                    else "."
                         <> text (TL.pack . ppNumSep' False $
                             replicate (negate expn) '0' -- zeroes after decimal
                             ++ concatMap show fracDigits))

-- | Just a number.
plainNum :: Double -> Doc
plainNum = ppNum False False False False

-- | Just a percentage.
plainPc :: Double -> Doc
plainPc = ppNum False True False False

roundNum' :: Bool -> Bool -> Double -> Doc
roundNum' is_pc pos_plus n =
    let rounded :: Int
        rounded = round n
    in ppNum False is_pc True pos_plus rounded

-- | Just a number, but make sure it's an integer by rounding it off.
roundNum :: Double -> Doc
roundNum = roundNum' False False

-- | Round number to nearest integer, and don't add spaces.
roundNumNoSpace :: (RealFrac n, PPSep n) => n -> Text
roundNumNoSpace n = doc2text $ integer (round n :: Integer)

-- | Just a percentage, but make sure it's an integer by rounding it off.
roundPc :: Double -> Doc
roundPc = roundNum' True False

-- | Colour a number green or red depending on whether it's "good" or "bad".
--
-- The first argument is True if positive is good, False if negative is good.
colourNumSign :: Bool -> Double -> Doc
colourNumSign good = ppNum True False good True

-- | As 'colourNumSign', but treat as a percentage (i.e. add a percent sign).
colourPcSign :: Bool -> Double -> Doc
colourPcSign good = ppNum True True good True

-- | As colourNum, but interpret the number as a percentage.
colourPc :: Bool -> Double -> Doc
colourPc good = ppNum True True good False

-- | As colourNum, but don't add a + at the start.
colourNum :: Bool -> Double -> Doc
colourNum good = ppNum True False good False

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
    let n_pp'd = (if pos_plus then pp_signed else pp_nosigned)
                 ppNumSep n <> if is_pc then "%" else ""
    in (if not colour then id else
        case (if pos then n else negate n) `compare` 0 of
            LT -> template "red" . (:[]) . doc2text
            EQ -> bold
            GT -> template "green" . (:[]) . doc2text)
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

-- | Say "Gain" or "Lose" depending on whether the numeric argument is positive
-- or negative (respectively).
--
-- XXX: These two (and all messages that use them) will have to change if this
-- is ever actually localized to anything besides English.
gainOrLose :: (Ord n, Num n) => n -> Text
gainOrLose n | n < 0     = "Lose"
             | otherwise = "Gain"

gainsOrLoses :: (Ord n, Num n) => n -> Text
gainsOrLoses n | n < 0     = "loses"
               | otherwise = "gains"

-----------------
---- Wiki text --
-----------------

-- | Template with arguments. @template "foo" ["bar","baz"]@ produces
-- @{{foo|bar|baz}}@.
template :: Text -> [Text] -> Doc
template name args = templateDoc (strictText name) (map strictText args)

-- | Doc version of 'template'.
templateDoc :: Doc -> [Doc] -> Doc
templateDoc name args = hcat $
    "{{"
    : (intersperse "|" (name:args)
      ++ ["}}"])

-- | Set text in italics, and wrap in quotation marks.
iquotes :: Text -> Doc
iquotes = enclose "''\"" "\"''" . strictText

---- Set doc in italics.
--italic :: Doc -> Doc
--italic = enclose "''" "''"

-- | Set doc in boldface.
bold :: Doc -> Doc
bold = enclose "'''" "'''"

boldText :: Text -> Text
boldText = doc2text . bold . strictText

-- Produce output based on a boolean (i.e. if-then-else).
-- Needed because the i18n templates don't understand this syntax, but instead
-- interpret these three keywords as identifiers.
ifThenElse :: Bool -> a -> a -> a
ifThenElse yn yes no = if yn then yes else no
-- Specialized versions, to help type inference
ifThenElseT :: Bool -> Text -> Text -> Text
ifThenElseT = ifThenElse
