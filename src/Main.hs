{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.IO.Encoding

import Debug.Trace

import Control.Exception
import Control.Monad
import Data.Maybe
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Data.IORef

import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Abstract
import Doc
import FileIO
import Platform
import Settings
import EU4.Feature

-- Script handlers
import EU4.Common
import EU4.Decisions
import EU4.Missions
import EU4.Events
import EU4.IdeaGroups
import EU4.Policies

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readScripts :: Settings extra -> FilePath -> IO [(FilePath, GenericScript)]
readScripts settings category =
    let sourceSubdir = case category of
            "policies" -> "common" </> "policies"
            "ideagroups" -> "common" </> "ideas"
            _          -> category
        sourceDir = buildPath settings sourceSubdir
    in do
        files <- filterM (doesFileExist . buildPath settings . (sourceSubdir </>)) =<< getDirectoryContents sourceDir
        forM files $ \filename -> do
            let target = sourceSubdir </> filename
            content <- readScript settings (buildPath settings target)
            when (null content) $
                hPutStrLn stderr $ "Warning: " ++ target ++ " contains no scripts - failed parse?"
            return (target, content)

data SomeFeature g = forall a. Feature g a => SomeFeature { feature :: a }

ppSomeFeature :: MonadError Text m => SomeFeature g -> PPT g m Doc
ppSomeFeature (SomeFeature x) = ppFeature x

features :: [SomeFeature EU4]
features = [SomeFeature (emptyFeature :: Decision)
           ,SomeFeature (emptyFeature :: Mission)
           ,SomeFeature (emptyFeature :: Event)
           ,SomeFeature (emptyFeature :: Policy)
           ,SomeFeature (emptyFeature :: IdeaGroup)]
readAllFeatures :: Settings () -> IO (Settings EU4)
readAllFeatures s = flip execStateT (fmap (const eu4) s) $
    forM_ features $ \(SomeFeature feature) -> do -- one feature
        settings <- get
        scripts <- liftIO (readScripts settings (featureDirectory feature))
        forM_ scripts $ \(path, script) -> do -- one file
            when (verbose s) $
                liftIO . putStrLn $ "Loading: " ++ path
            forM_ script $ \stmt -> do -- one "line" of a file, may contain several features
                case (flip runReaderT settings .
                                setCurrentFile path $ readFeatures stmt)
                        `asTypeOf` Right [Right (Just feature)] of
                    Left err -> liftIO . putStrLn $
                        "Error processing " ++ featureDirectory feature ++ ": " ++ T.unpack err
                    Right things -> forM_ things $ \case
                        Left err -> liftIO . putStrLn $
                            "Error processing " ++ path ++ ": " ++ T.unpack err
                        Right Nothing -> return ()
                        Right (Just thing) -> do
                            modify (\s -> s { info = loadFeature thing (info s) })

main :: IO ()
main = do
    -- Do platform-specific initialization
    initPlatform

    -- Read settings, then read all files.
    settings <- readAllFeatures =<< readSettings

    -- Then, output everything.
    let results :: Either Text [Table (FilePath, Either Text (FilePath, Doc))]
        results = flip runReaderT settings . forM features $ \(SomeFeature feature) -> do
            things <- getFeatures feature <$> asks info
            forM things $ \thing -> fmap ((,) (featureDirectory feature)) $
                -- PPT EU4 m (Either Text Doc)
                (Right . (,) (featurePath thing) <$> ppFeature thing)
                `catchError` \err -> return (Left err)

    createDirectoryIfMissing False "output"

    putStrLn "Processing..."

    case results of
        Left err -> void . putStrLn $ "Failed processing: " ++ T.unpack err
        Right tables -> forM_ tables $ \table -> do
            let total = HM.size table
                total_s = show total
                total_w = length total_s
            counter <- newIORef 1
            forM_ table $ \(path, result) -> do
                nth <- readIORef counter
                when (verbose settings) $ do
                    counter `modifyIORef` succ
                    when (nth == 1) $
                        putStrLn $ "Processing " ++ show total ++ " " ++
                                let comps = splitPath path
                                 in joinPath (take (length comps - 2) comps)
                    let nth_s = show nth
                        nth_w = length nth_s
                    putStr $ replicate (total_w - nth_w) ' ' ++ nth_s ++ "/" ++ total_s ++ ": "
                case result of
                    Left err -> void . putStrLn $
                        "Processing " ++ path ++ " failed: " ++ T.unpack err
                    Right (target, output) -> do
                        when (verbose settings) $
                            putStrLn $ show nth ++ "/" ++ show total ++ ": " ++ target
                        let destinationFile = "output" </> target
                            destinationDir  = takeDirectory destinationFile
                        createDirectoryIfMissing True destinationDir
                        h <- openFile destinationFile WriteMode
                        result <- try $
                            displayIO h (renderPretty 0.9 80 output)
                        case result of
                            Right () -> return ()
                            Left err -> hPutStrLn stderr $
                                "Error writing " ++ show (err::IOError)
                        hClose h
