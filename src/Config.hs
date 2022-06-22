{-# LANGUAGE OverloadedStrings #-}

module Config where

-- Top-level command options for getting a help document.
-- Need to change in certain cases:
--    ex) helpOptions = [[""]] -- For seqtk
helpOptions :: [[String]]
helpOptions = [["--help"], ["help"], ["-help"], ["-h"]]

-- Subcommand-level options for getting a help document.
-- Need to change in certain cases:
--    ex) helpOptionsSub subname = [[subname]] -- For seqtk
helpOptionsSub :: String -> [[String]]
helpOptionsSub subname = [[subname, "--help"], ["help", subname], [subname, "-help"], [subname, "-h"]]
