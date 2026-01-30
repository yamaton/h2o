{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- | Global verbosity flag, set at startup
{-# NOINLINE verboseRef #-}
verboseRef :: IORef Bool
verboseRef = unsafePerformIO (newIORef False)

setVerbose :: Bool -> IO ()
setVerbose = writeIORef verboseRef

isVerbose :: Bool
isVerbose = unsafePerformIO (readIORef verboseRef)

-- | Top-level command option for getting a help document.
-- Need to change in certain cases:
--    ex) helpOptions = [[""]] -- For seqtk
helpOptions :: [[String]]
helpOptions = [["--help"], ["help"], ["-help"], ["-h"], [" "]]

-- | Subcommand-level options for getting a help document.
-- Need to change in certain cases:
--    ex) helpOptionsSub subname = [[subname]] -- For seqtk
helpOptionsSub :: String -> [[String]]
helpOptionsSub subname = [[subname, "--help"], ["help", subname], [subname, "-help"], [subname, "-h"], [subname]]

-- | Command option to get a version
versionOptions :: [[String]]
versionOptions = [["--version"], ["version"], ["-version"]]
