{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Messages (
        colorNum
    ,   pp_hl_num
    ,   pp_hl_pc
    ,   pp_num_sep
    ,   template
    ,   bold, italic
    ,   ifThenElse
    ,   ScriptMessage (..)
    ,   message, messageText
    ,   NumType (..)
    ,   adjustNumber 
    ,   module Doc
    ) where

import Control.Applicative ((<$>))

import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.Shakespeare.I18N
import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), int, double)

import Numeric (floatToDigits)

import Doc
import SettingsTypes

----------------------
-- Printing numbers --
----------------------

data NumType         -- Treat 1 as:
    = Plain          -- 1
    | Reduced        -- 100
    deriving (Show, Eq, Ord)

adjustNumber :: Num n => NumType -> n -> n
adjustNumber Plain          n = n
adjustNumber Reduced        n = n * 100

-- Pretty-print a number, adding &#8239; (U+202F NARROW NO-BREAK SPACE) at
-- every power of 1000.
class Num a => PPSep a where
    pp_num_sep :: a -> Doc

group3 :: [a] -> [[a]]
group3 = unfoldr (\cs -> if null cs then Nothing else Just (splitAt 3 cs))

instance PPSep Integer where
    pp_num_sep n = strictText . T.pack $
            (if n < 0 then "-" else "") <> pp_num_sep' True (show (abs n))

-- Split into groups of 3 and intersperse the groups with narrow no-break
-- spaces.
-- If first arg is True, start grouping at the end (e.g. for integers).
pp_num_sep' :: Bool -> String -> String
pp_num_sep' int
    = mconcat
        . (if int then reverse else id)
        . intersperse "&#8239;"
        . (if int then map reverse else id)
        . group3 
        . (if int then reverse else id)

instance PPSep Int where
    pp_num_sep = pp_num_sep . toInteger

instance PPSep Double where
    pp_num_sep n
        = let absn = abs n
              (digits, exp) = floatToDigits 10 absn
              (_, fracDigits') = splitAt exp digits
              -- fracDigits' is [] if exp is a nonzero whole number
              fracDigits = if fracDigits' == [0] then [] else fracDigits'
          in (if n < 0 then "-" else "")
                <> text (TL.pack . pp_num_sep' True $ show (truncate absn))
                <> (if null fracDigits
                    then ""
                    else "."
                         <> text (TL.pack . pp_num_sep' False $
                             replicate (negate exp) '0' -- zeroes after decimal
                             ++ concatMap show fracDigits))

-- Pretty-print a number, adding wiki formatting:
-- * {{green}} if good
-- * {{red}} if bad
-- * '''boldface''' if neutral
-- What is good or bad is determined by the first argument:
-- * if True, positive is good and negative is bad (e.g. stability)
-- * if False, negative is good and positive is bad (e.g. inflation)
-- * Either way, zero is neutral.
pp_hl_num :: (Ord n, PPSep n) => Bool -> n -> Doc
pp_hl_num = pp_hl_num' False

-- Text version
colorNum :: (Ord n, PPSep n) => Bool -> n -> Text
colorNum good n = doc2text (pp_hl_num good n)

-- No colour, just render the number
plainNum :: PPSep n => n -> Text
plainNum = doc2text . pp_num_sep

-- No colour, just render the number. Round it to nearest integer.
roundNum :: (RealFrac n, PPSep n) => n -> Text
roundNum n = doc2text $ pp_num_sep (round n :: Integer)

adjustedNum :: PPSep n => NumType -> (n -> Text) -> n -> Text
adjustedNum numtype p n = p (adjustNumber numtype n)

-- Format a number as a percentage (e.g. 0.5 -> 50%)
pp_hl_pc :: (Show n, Ord n, PPSep n) => Bool -> n -> Doc
pp_hl_pc = pp_hl_num' True

pp_hl_num' :: (Ord n, PPSep n) => Bool -> Bool -> n -> Doc
pp_hl_num' is_pc pos n =
    let sign = signum n
        positivity = if pos then sign else negate sign
        n_pp'd = pp_signed pp_num_sep n <> if is_pc then "%" else ""
    in case positivity of
        -1 -> template "red" [doc2text n_pp'd]
        0 ->  bold n_pp'd
        1 ->  template "green" [doc2text n_pp'd]

plural :: (Eq n, Num n) => n -> Text -> Text -> Text
plural n sing plur | n == 1    = sing
                   | otherwise = plur

-- XXX: These two (and all messages that use them) will have to change if this
-- is ever actually localized to anything besides English.
gainOrLose :: (Ord n, Num n) => n -> Text
gainOrLose n | n < 0     = "Lose"
             | otherwise = "Gain"

gainsOrLoses :: (Ord n, Num n) => n -> Text
gainsOrLoses n | n < 0     = "loses"
               | otherwise = "gains"

---------------
-- Wiki text --
---------------

-- Template with args.
-- TODO: Escaping of pipes (i.e. replacing them with {{!}})
template :: Text -> [Text] -> Doc
template name args = hcat . map strictText $
    "{{"
    : (intersperse "|" (name:args)
      ++ ["}}"])

-- Set doc in italics.
italic :: Doc -> Doc
italic content = enclose "''" "''" content

-- Set doc in boldface.
bold :: Doc -> Doc
bold content = enclose "'''" "'''" content

-- Produce output based on a boolean (i.e. if-then-else).
-- Needed because the i18n templates don't understand this syntax, but instead
-- interpret these three keywords as identifiers.
ifThenElse :: Bool -> a -> a -> a
ifThenElse yn yes no = if yn then yes else no
-- Specialized versions, to help type inference
ifThenElseT :: Bool -> Text -> Text -> Text
ifThenElseT = ifThenElse

--------------
-- Messages --
--------------

-- dummy type required by the Shakespeare machinery
data Script = Script

-- The following splice generates the ScriptMessage type and a RenderMessage
-- instance for it.
mkMessage "Script" "l10n" "en"

messageText :: ScriptMessage -> PP extra Text
messageText msg = do
    langs <- getLangs
    return $ renderMessage Script langs msg

message :: ScriptMessage -> PP extra Doc
message msg = strictText <$> messageText msg
