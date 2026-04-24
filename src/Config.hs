{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- $verbose
--
-- = Process-wide verbose flag (a deliberate compromise)
--
-- The trace helpers in "Utils" ('debugMsg', 'infoTrace', 'warnShow', …)
-- are typed as @a -> a@ so they can be sprinkled inside parser
-- combinators without forcing every caller into 'IO' or threading a
-- 'ReaderT' context. To make those helpers respect a runtime
-- @--verbose@ switch they consult this single global flag via
-- 'isVerbose' (which performs an 'unsafePerformIO' read of an
-- 'IORef').
--
-- == Conventions / limitations
--
--   * 'setVerbose' /must/ be called before any code that may evaluate
--     'isVerbose'. The bundled CLI ("Main") satisfies this by calling
--     'setVerbose' inside 'initialize', strictly before 'Io.run'.
--     Library callers should follow the same pattern.
--   * 'isVerbose' is intended for /pure/ contexts (the trace helpers).
--     IO callers should prefer 'getVerbose' for an honest 'IO Bool'
--     read.
--   * Running with distinct verbose settings concurrently from the
--     same process is unsupported - this flag is global, not per-call.
--
-- A full removal of this global would require either threading a
-- 'ReaderT' through every IO entry point /and/ giving the pure trace
-- helpers either an explicit 'Bool' argument or an
-- @-XImplicitParams@-style @(?verbose :: Bool)@ constraint, and then
-- updating every transitive caller's type signature. That refactor
-- spans many files; this comment is the marker that the current
-- design is a known compromise that has so far been adequate for the
-- single-shot CLI use case.

-- | Mutable storage for the verbose flag. NOINLINE keeps GHC from
-- duplicating or sharing the underlying 'IORef', which would defeat
-- the global-flag semantics.
{-# NOINLINE verboseRef #-}
verboseRef :: IORef Bool
verboseRef = unsafePerformIO (newIORef False)

-- | Set the verbose flag. Call this exactly once during program
-- startup, before invoking any code that consults 'isVerbose'.
setVerbose :: Bool -> IO ()
setVerbose = writeIORef verboseRef

-- | Read the verbose flag from /pure/ context. NOINLINE prevents GHC
-- from inlining (and thereby caching) the result of the
-- 'unsafePerformIO' read at use sites; combined with the
-- 'setVerbose'-first convention, this is what keeps the trace helpers
-- honest.
{-# NOINLINE isVerbose #-}
isVerbose :: Bool
isVerbose = unsafePerformIO (readIORef verboseRef)

-- | The honest IO accessor. Prefer this whenever you are already in
-- 'IO'; it does not need the 'unsafePerformIO' wrapper that
-- 'isVerbose' relies on.
getVerbose :: IO Bool
getVerbose = readIORef verboseRef

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
