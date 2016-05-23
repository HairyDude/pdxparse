module FileIO (
        readFileRetry
    ,   buildPath
    ,   readScript
    ,   module System.IO
    ) where

import Control.Exception

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Text.Encoding as TE

import System.FilePath
import System.IO

import qualified Data.Attoparsec.Text as Ap

import Debug.Trace

import Abstract
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
    case Ap.parseOnly (skipSpace >> genericScript) contents of
        Right result -> return result
        Left error -> do
            putStrLn $ "Couldn't parse " ++ file ++ ": " ++ error
            return []
