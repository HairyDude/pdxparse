{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module FileIO (
        readFileRetry
    ,   buildPath
    ,   readScript
    ,   Feature (..)
    ,   writeFeatures
    ,   module System.IO
    ) where

import Control.Monad
import Control.Monad.Except
import Control.Exception

import Data.Monoid

import qualified Data.ByteString as B

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.Encoding as TE

import System.Directory
import System.FilePath
import System.IO

import qualified Data.Attoparsec.Text as Ap

import Abstract
import Doc
import SettingsTypes

-- Read a file as Text. Unfortunately EU4 script files use several incompatible
-- encodings. Try the following encodings in order:
-- 1. UTF-8
-- 2. ISO 8859-1
-- (Decoding as 8859-1 can't fail, but I don't know if it will always be correct.)
readFileRetry :: FilePath -> IO Text
readFileRetry path = do
    raw <- B.readFile path
    -- Catching exceptions in pure code is a rather convoluted process...
    e <- try (let e = TE.decodeUtf8 raw in e `seq` return e)
    case (e::Either UnicodeException Text) of
        Right result -> return result
        Left _ -> return $ TE.decodeLatin1 raw

buildPath :: Settings -> FilePath -> FilePath
buildPath settings path = steamDir settings </> steamApps settings </> gameFolder settings </> path

-------------------------------
-- Reading scripts from file --
-------------------------------

readScript :: Settings -> FilePath -> IO GenericScript
readScript settings file = do
    let filepath = buildPath settings file
    contents <- readFileRetry filepath
    case Ap.parseOnly (Ap.option undefined (Ap.char '\xFEFF') -- BOM
                        *> skipSpace
                        *> genericScript) contents of
        Right result -> return result
        Left err -> do
            putStrLn $ "Couldn't parse " ++ file ++ ": " ++ err
            return []

------------------------------
-- Writing features to file --
------------------------------

data Feature a = Feature {
        featureId :: Maybe Text
    ,   featurePath :: Maybe FilePath
    ,   theFeature :: Either Text a
    } deriving (Show)

writeFeature :: FilePath -> Doc -> IO ()
writeFeature path output = do
    let destinationFile = "output" </> path
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

writeFeatures :: Text -- ^ Name of feature (e.g. "idea groups")
    -> [Feature a]
    -> (a -> PPT (ExceptT Text IO) Doc) -- ^ Rendering function
    -- PPT (ExceptT Text IO) = StateT Settings (ReaderT GameState (ExceptT Text IO))
    -> PPT IO ()
writeFeatures featureName features pprint = do
    efeatures_pathed_pp'd <- forM features $ \feature ->
        case theFeature feature of
            Left err ->
                -- Error was passed to us - report it
                return (feature {
                        theFeature = Left $ "Error while parsing" <> featureName <> ":" <> err
                    })
            Right thing -> case (featurePath feature, featureId feature) of
                (Just _, Just _) -> do
                    doc <- hoistExceptions (pprint thing)
                    return (feature { theFeature = Right doc })
                (Nothing, Nothing) -> return (feature {
                        theFeature = Left $ "Error while writing " <> featureName
                                        <> ": missing path and id"
                    })
                (Nothing, Just oid) -> return (feature {
                        theFeature = Left $ "Error while writing " <> featureName
                                        <> " " <> oid <> ": missing path"
                    })
                (Just path, Nothing) -> return (feature {
                        theFeature = Left $ "Error while writing " <> featureName
                                        <> " " <> T.pack path <> ": missing id"
                    })
    liftIO $ forM_ efeatures_pathed_pp'd $ \feature -> case theFeature feature of
        Right (Right output) -> case (featurePath feature, featureId feature) of
            (Just sourcePath, Just feature_id) ->
                writeFeature (sourcePath </> T.unpack feature_id) output
            (Just sourcePath, Nothing) -> liftIO . TIO.putStrLn $
                "Error while writing " <> featureName <> " in " <> T.pack sourcePath <> ": missing id"
            (Nothing, Just fid) -> liftIO . TIO.putStrLn $
                "Error while writing " <> featureName <> " " <> fid <> ": missing source path"
            (Nothing, Nothing) -> liftIO . TIO.putStrLn $
                "Error while writing " <> featureName <> ": missing source path and id"
        e -> TIO.putStrLn (eitherError e) where
            eitherError (Right (Right _)) = error "impossible: fall through in writeFeatures"
            eitherError (Right (Left err)) = err
            eitherError (Left err) = err
    -- TODO: within each input file, sort by id, then write to a
    -- consolidated file.

