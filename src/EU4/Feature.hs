{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module EU4.Feature (
        Feature (..)
    ,   module Abstract
    ) where

import Control.Monad.Except (MonadError)

import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text (Doc)

import System.FilePath (FilePath)

import Abstract (GenericStatement)
import SettingsTypes

class Feature g a | a -> g where
    emptyFeature :: a
    featureDirectory :: a -> FilePath -- dummy arg, should be constant
    featurePath :: a -> FilePath
    readFeatures :: MonadError Text m => GenericStatement -> PPT g m [Either Text (Maybe a)]
    loadFeature :: a -> g -> g
    getFeatures :: a -> g -> Table a -- dummy arg, should be constant
    ppFeature :: MonadError Text m => a -> PPT g m Doc
