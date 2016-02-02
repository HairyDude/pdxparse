module Platform (initPlatform) where

import GHC.IO.Encoding
import System.Win32.Console

initPlatform :: IO ()
initPlatform = do
    setLocaleEncoding utf8
    setConsoleOutputCP 65001
