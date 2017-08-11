{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-|
Module      : FileIO
Description : High level I/O for Clausewitz scripts
-}
module FileIO (
        readFileRetry
    ,   buildPath
    ,   readScript
    ,   Feature (..)
    ,   writeFeatures
    ) where

import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadIO (..))
import Control.Exception (try)

import Data.Monoid ((<>))

import qualified Data.ByteString as B

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.Encoding as TE

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (withFile, IOMode (..), hPutStrLn, stderr)

import qualified Data.Attoparsec.Text as Ap
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import SettingsTypes (Settings (..), PPT, hoistExceptions)

-- | Read a file as Text. Unfortunately EU4 script files use several incompatible
-- encodings. Try the following encodings in order:
--
-- 1. UTF-8
-- 2. ISO 8859-1
--
-- (Decoding as 8859-1 can't fail, but I don't know if it will always be correct.)
readFileRetry :: FilePath -> IO Text
readFileRetry path = do
    raw <- B.readFile path
    -- Catching exceptions in pure code is a rather convoluted process...
    e <- try (let e = TE.decodeUtf8 raw in e `seq` return e)
    case (e::Either UnicodeException Text) of
        Right result -> return result
        Left _ -> return $ TE.decodeLatin1 raw

-- | Given a path under the game's root directory, build a fully qualified path
-- referring to that file.
--
-- For example, if we're parsing EU4 on Windows with the usual install
-- location:
--
-- @
--  buildPath settings "events/FlavorENG.txt" = "C:\Program Files (x86)\Steam\steamapps\common\Europa Universalis IV\events\FlavorENG.txt"
-- @
buildPath :: Settings -> FilePath -> FilePath
buildPath settings path = steamDir settings </> steamApps settings </> gameFolder settings </> path

-------------------------------
-- Reading scripts from file --
-------------------------------

-- | Read and parse a script file. On error, report to standard output and
-- return an empty script.
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

-- | An individual game feature. For example, a value for this exists for each
-- EU4 event, one for each idea group, one for each decision, etc.
--
-- The parameter is a type containing data relevant to that feature, or an
-- error message from processing.
data Feature a = Feature {
        featureId :: Maybe Text
    ,   featurePath :: Maybe FilePath
    ,   theFeature :: Either Text a
    } deriving (Show)

-- TODO: allow writing to a different output directory
-- | Write a parsed and presented feature to the given file under the directory
-- @./output@. If the filename includes directories, create them first.
writeFeature :: FilePath -> Doc -> IO ()
writeFeature path output = do
    let destinationFile = "output" </> path
        destinationDir  = takeDirectory destinationFile
    createDirectoryIfMissing True destinationDir
    withFile destinationFile WriteMode $ \h -> do
        result <- try $
            PP.displayIO h (PP.renderPretty 0.9 80 output)
        case result of
            Right () -> return ()
            Left err -> hPutStrLn stderr $
                "Error writing " ++ show (err::IOError)

-- | Given a list of features, present them and output to the appropriate files
-- under the directory @./output@.
writeFeatures :: MonadIO m =>
    Text -- ^ Name of feature (e.g. "idea groups")
        -> [Feature a]
        -> (a -> PPT g (ExceptT Text m) Doc) -- ^ Rendering function
        -- PPT g (ExceptT Text IO) = StateT Settings (ReaderT GameState (ExceptT Text IO))
        -> PPT g m ()
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

