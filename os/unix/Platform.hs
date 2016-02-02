module Platform (initPlatform) where

-- Nothing to do for Linux
initPlatform :: IO ()
initPlatform = return ()
