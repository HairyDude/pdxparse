module Localization
        ( readL10n -- :: Settings -> IO L10n
        , module SettingsTypes
        ) where

import Control.Monad
import Data.Monoid

import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)

import SettingsTypes

import Yaml

import Debug.Trace

readL10n :: Settings a -> IO L10n
readL10n settings = do
    let dir = steamDir settings
              </> steamApps settings
              </> game settings
              </> "localisation"
    files <- filterM doesFileExist
                . map (dir </>)
                    =<< getDirectoryContents dir
    liftM (foldl' mergeLangs HM.empty) . forM files $ \file -> do
        parseResult <- parseLocFile <$> TIO.readFile file
        case parseResult of
            Left exc -> do
                hPutStrLn stderr $ "Parsing localisation file " ++ file ++ " failed: " ++ show exc
                return HM.empty
            Right contents -> return contents
