-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2001-2003
--
-- Access to system tools: gcc, cp, rm etc
--
-----------------------------------------------------------------------------

\begin{code}
module SysTools (
        -- Initialisation
        initSysTools,

        -- Interface to system tools
        runUnlit, runCpp, runCc, -- [Option] -> IO ()
        runPp,                   -- [Option] -> IO ()
        runSplit,                -- [Option] -> IO ()
        runAs, runLink, runLibtool, -- [Option] -> IO ()
        runMkDLL,
        runWindres,
        runLlvmOpt,
        runLlvmLlc,
        runClang,
        figureLlvmVersion,
        readElfSection,

        getLinkerInfo,
        getCompilerInfo,

        linkDynLib,

        askCc,

        touch,                  -- String -> String -> IO ()
        copy,
        copyWithHeader,

        -- Temporary-file management
        setTmpDir,
        newTempName,
        cleanTempDirs, cleanTempFiles, cleanTempFilesExcept,
        addFilesToClean,

        Option(..)

 ) where

#include "HsVersions.h"

import DriverPhases
import Module
import Packages
import Config
import Outputable
import ErrUtils
import Panic
import Platform
import Util
import DynFlags
import Exception

import Data.IORef
import Control.Monad
import System.Exit
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error as IO
import System.Directory
import Data.Char
import Data.List
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP hiding (char)
import qualified Text.ParserCombinators.ReadP as R

#ifndef mingw32_HOST_OS
import qualified System.Posix.Internals
#else /* Must be Win32 */
import Foreign
import Foreign.C.String
#endif

import System.Process
import Control.Concurrent
import FastString
import SrcLoc           ( SrcLoc, mkSrcLoc, noSrcSpan, mkSrcSpan )

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif
\end{code}

How GHC finds its files
~~~~~~~~~~~~~~~~~~~~~~~

[Note topdir]

GHC needs various support files (library packages, RTS etc), plus
various auxiliary programs (cp, gcc, etc).  It starts by finding topdir,
the root of GHC's support files

On Unix:
  - ghc always has a shell wrapper that passes a -B<dir> option

On Windows:
  - ghc never has a shell wrapper.
  - we can find the location of the ghc binary, which is
        $topdir/bin/<something>.exe
    where <something> may be "ghc", "ghc-stage2", or similar
  - we strip off the "bin/<something>.exe" to leave $topdir.

from topdir we can find package.conf, ghc-asm, etc.


SysTools.initSysProgs figures out exactly where all the auxiliary programs
are, and initialises mutable variables to make it easy to call them.
To to this, it makes use of definitions in Config.hs, which is a Haskell
file containing variables whose value is figured out by the build system.

Config.hs contains two sorts of things

  cGCC,         The *names* of the programs
  cCPP            e.g.  cGCC = gcc
  cUNLIT                cCPP = gcc -E
  etc           They do *not* include paths


  cUNLIT_DIR   The *path* to the directory containing unlit, split etc
  cSPLIT_DIR   *relative* to the root of the build tree,
                   for use when running *in-place* in a build tree (only)



---------------------------------------------
NOTES for an ALTERNATIVE scheme (i.e *not* what is currently implemented):

Another hair-brained scheme for simplifying the current tool location
nightmare in GHC: Simon originally suggested using another
configuration file along the lines of GCC's specs file - which is fine
except that it means adding code to read yet another configuration
file.  What I didn't notice is that the current package.conf is
general enough to do this:

Package
    {name = "tools",    import_dirs = [],  source_dirs = [],
     library_dirs = [], hs_libraries = [], extra_libraries = [],
     include_dirs = [], c_includes = [],   package_deps = [],
     extra_ghc_opts = ["-pgmc/usr/bin/gcc","-pgml${topdir}/bin/unlit", ... etc.],
     extra_cc_opts = [], extra_ld_opts = []}

Which would have the advantage that we get to collect together in one
place the path-specific package stuff with the path-specific tool
stuff.
                End of NOTES
---------------------------------------------

%************************************************************************
%*                                                                      *
\subsection{Initialisation}
%*                                                                      *
%************************************************************************

\begin{code}
initSysTools :: Maybe String    -- Maybe TopDir path (without the '-B' prefix)
             -> IO Settings     -- Set all the mutable variables above, holding
                                --      (a) the system programs
                                --      (b) the package-config file
                                --      (c) the GHC usage message
initSysTools mbMinusB
  = do top_dir <- findTopDir mbMinusB
             -- see [Note topdir]
             -- NB: top_dir is assumed to be in standard Unix
             -- format, '/' separated

       let settingsFile = top_dir </> "settings"
           platformConstantsFile = top_dir </> "platformConstants"
           installed :: FilePath -> FilePath
           installed file = top_dir </> file

       settingsStr <- readFile settingsFile
       platformConstantsStr <- readFile platformConstantsFile
       mySettings <- case maybeReadFuzzy settingsStr of
                     Just s ->
                         return s
                     Nothing ->
                         pgmError ("Can't parse " ++ show settingsFile)
       platformConstants <- case maybeReadFuzzy platformConstantsStr of
                            Just s ->
                                return s
                            Nothing ->
                                pgmError ("Can't parse " ++
                                          show platformConstantsFile)
       let getSetting key = case lookup key mySettings of
                            Just xs ->
                                return $ case stripPrefix "$topdir" xs of
                                         Just [] ->
                                             top_dir
                                         Just xs'@(c:_)
                                          | isPathSeparator c ->
                                             top_dir ++ xs'
                                         _ ->
                                             xs
                            Nothing -> pgmError ("No entry for " ++ show key ++ " in " ++ show settingsFile)
           getBooleanSetting key = case lookup key mySettings of
                                   Just "YES" -> return True
                                   Just "NO" -> return False
                                   Just xs -> pgmError ("Bad value for " ++ show key ++ ": " ++ show xs)
                                   Nothing -> pgmError ("No entry for " ++ show key ++ " in " ++ show settingsFile)
           readSetting key = case lookup key mySettings of
                             Just xs ->
                                 case maybeRead xs of
                                 Just v -> return v
                                 Nothing -> pgmError ("Failed to read " ++ show key ++ " value " ++ show xs)
                             Nothing -> pgmError ("No entry for " ++ show key ++ " in " ++ show settingsFile)
       targetArch <- readSetting "target arch"
       targetOS <- readSetting "target os"
       targetWordSize <- readSetting "target word size"
       targetUnregisterised <- getBooleanSetting "Unregisterised"
       targetHasGnuNonexecStack <- readSetting "target has GNU nonexec stack"
       targetHasIdentDirective <- readSetting "target has .ident directive"
       targetHasSubsectionsViaSymbols <- readSetting "target has subsections via symbols"
       myExtraGccViaCFlags <- getSetting "GCC extra via C opts"
       -- On Windows, mingw is distributed with GHC,
       -- so we look in TopDir/../mingw/bin
       -- It would perhaps be nice to be able to override this
       -- with the settings file, but it would be a little fiddly
       -- to make that possible, so for now you can't.
       gcc_prog <- getSetting "C compiler command"
       gcc_args_str <- getSetting "C compiler flags"
       let unreg_gcc_args = if targetUnregisterised
                            then ["-DNO_REGS", "-DUSE_MINIINTERPRETER"]
                            else []
           -- TABLES_NEXT_TO_CODE affects the info table layout.
           tntc_gcc_args
            | mkTablesNextToCode targetUnregisterised
               = ["-DTABLES_NEXT_TO_CODE"]
            | otherwise = []
           gcc_args = map Option (words gcc_args_str
                               ++ unreg_gcc_args
                               ++ tntc_gcc_args)
       ldSupportsCompactUnwind <- getBooleanSetting "ld supports compact unwind"
       ldSupportsBuildId       <- getBooleanSetting "ld supports build-id"
       ldSupportsFilelist      <- getBooleanSetting "ld supports filelist"
       ldIsGnuLd               <- getBooleanSetting "ld is GNU ld"
       perl_path <- getSetting "perl command"

       let pkgconfig_path = installed "package.conf.d"
           ghc_usage_msg_path  = installed "ghc-usage.txt"
           ghci_usage_msg_path = installed "ghci-usage.txt"

             -- For all systems, unlit, split, mangle are GHC utilities
             -- architecture-specific stuff is done when building Config.hs
           unlit_path = installed cGHC_UNLIT_PGM

             -- split is a Perl script
           split_script  = installed cGHC_SPLIT_PGM

       windres_path <- getSetting "windres command"
       libtool_path <- getSetting "libtool command"

       tmpdir <- getTemporaryDirectory

       touch_path <- getSetting "touch command"

       let -- On Win32 we don't want to rely on #!/bin/perl, so we prepend
           -- a call to Perl to get the invocation of split.
           -- On Unix, scripts are invoked using the '#!' method.  Binary
           -- installations of GHC on Unix place the correct line on the
           -- front of the script at installation time, so we don't want
           -- to wire-in our knowledge of $(PERL) on the host system here.
           (split_prog,  split_args)
             | isWindowsHost = (perl_path,    [Option split_script])
             | otherwise     = (split_script, [])
       mkdll_prog <- getSetting "dllwrap command"
       let mkdll_args = []

       -- cpp is derived from gcc on all platforms
       -- HACK, see setPgmP below. We keep 'words' here to remember to fix
       -- Config.hs one day.
       let cpp_prog  = gcc_prog
           cpp_args  = Option "-E"
                     : map Option (words cRAWCPP_FLAGS)
                    ++ gcc_args

       -- Other things being equal, as and ld are simply gcc
       gcc_link_args_str <- getSetting "C compiler link flags"
       let   as_prog  = gcc_prog
             as_args  = gcc_args
             ld_prog  = gcc_prog
             ld_args  = gcc_args ++ map Option (words gcc_link_args_str)

       -- We just assume on command line
       lc_prog <- getSetting "LLVM llc command"
       lo_prog <- getSetting "LLVM opt command"

       let platform = Platform {
                          platformArch = targetArch,
                          platformOS   = targetOS,
                          platformWordSize = targetWordSize,
                          platformUnregisterised = targetUnregisterised,
                          platformHasGnuNonexecStack = targetHasGnuNonexecStack,
                          platformHasIdentDirective = targetHasIdentDirective,
                          platformHasSubsectionsViaSymbols = targetHasSubsectionsViaSymbols
                      }

       return $ Settings {
                    sTargetPlatform = platform,
                    sTmpDir         = normalise tmpdir,
                    sGhcUsagePath   = ghc_usage_msg_path,
                    sGhciUsagePath  = ghci_usage_msg_path,
                    sTopDir         = top_dir,
                    sRawSettings    = mySettings,
                    sExtraGccViaCFlags = words myExtraGccViaCFlags,
                    sSystemPackageConfig = pkgconfig_path,
                    sLdSupportsCompactUnwind = ldSupportsCompactUnwind,
                    sLdSupportsBuildId       = ldSupportsBuildId,
                    sLdSupportsFilelist      = ldSupportsFilelist,
                    sLdIsGnuLd               = ldIsGnuLd,
                    sPgm_L   = unlit_path,
                    sPgm_P   = (cpp_prog, cpp_args),
                    sPgm_F   = "",
                    sPgm_c   = (gcc_prog, gcc_args),
                    sPgm_s   = (split_prog,split_args),
                    sPgm_a   = (as_prog, as_args),
                    sPgm_l   = (ld_prog, ld_args),
                    sPgm_dll = (mkdll_prog,mkdll_args),
                    sPgm_T   = touch_path,
                    sPgm_sysman  = top_dir ++ "/ghc/rts/parallel/SysMan",
                    sPgm_windres = windres_path,
                    sPgm_libtool = libtool_path,
                    sPgm_lo  = (lo_prog,[]),
                    sPgm_lc  = (lc_prog,[]),
                    -- Hans: this isn't right in general, but you can
                    -- elaborate it in the same way as the others
                    sOpt_L       = [],
                    sOpt_P       = [],
                    sOpt_F       = [],
                    sOpt_c       = [],
                    sOpt_a       = [],
                    sOpt_l       = [],
                    sOpt_windres = [],
                    sOpt_lo      = [],
                    sOpt_lc      = [],
                    sPlatformConstants = platformConstants
             }
\end{code}

\begin{code}
-- returns a Unix-format path (relying on getBaseDir to do so too)
findTopDir :: Maybe String -- Maybe TopDir path (without the '-B' prefix).
           -> IO String    -- TopDir (in Unix format '/' separated)
findTopDir (Just minusb) = return (normalise minusb)
findTopDir Nothing
    = do -- Get directory of executable
         maybe_exec_dir <- getBaseDir
         case maybe_exec_dir of
             -- "Just" on Windows, "Nothing" on unix
             Nothing  -> throwGhcExceptionIO (InstallationError "missing -B<dir> option")
             Just dir -> return dir
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Running an external program}
%*                                                                      *
%************************************************************************


\begin{code}
runUnlit :: DynFlags -> [Option] -> IO ()
runUnlit dflags args = do
  let prog = pgm_L dflags
      opts = getOpts dflags opt_L
  runSomething dflags "Literate pre-processor" prog
               (map Option opts ++ args)

runCpp :: DynFlags -> [Option] -> IO ()
runCpp dflags args =   do
  let (p,args0) = pgm_P dflags
      args1 = map Option (getOpts dflags opt_P)
      args2 = if gopt Opt_WarnIsError dflags
                 then [Option "-Werror"]
                 else []
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id  "C pre-processor" p
                       (args0 ++ args1 ++ args2 ++ args) mb_env

runPp :: DynFlags -> [Option] -> IO ()
runPp dflags args =   do
  let prog = pgm_F dflags
      opts = map Option (getOpts dflags opt_F)
  runSomething dflags "Haskell pre-processor" prog (args ++ opts)

runCc :: DynFlags -> [Option] -> IO ()
runCc dflags args =   do
  let (p,args0) = pgm_c dflags
      args1 = map Option (getOpts dflags opt_c)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags cc_filter "C Compiler" p args2 mb_env
 where
  -- discard some harmless warnings from gcc that we can't turn off
  cc_filter = unlines . doFilter . lines

  {-
  gcc gives warnings in chunks like so:
      In file included from /foo/bar/baz.h:11,
                       from /foo/bar/baz2.h:22,
                       from wibble.c:33:
      /foo/flibble:14: global register variable ...
      /foo/flibble:15: warning: call-clobbered r...
  We break it up into its chunks, remove any call-clobbered register
  warnings from each chunk, and then delete any chunks that we have
  emptied of warnings.
  -}
  doFilter = unChunkWarnings . filterWarnings . chunkWarnings []
  -- We can't assume that the output will start with an "In file inc..."
  -- line, so we start off expecting a list of warnings rather than a
  -- location stack.
  chunkWarnings :: [String] -- The location stack to use for the next
                            -- list of warnings
                -> [String] -- The remaining lines to look at
                -> [([String], [String])]
  chunkWarnings loc_stack [] = [(loc_stack, [])]
  chunkWarnings loc_stack xs
      = case break loc_stack_start xs of
        (warnings, lss:xs') ->
            case span loc_start_continuation xs' of
            (lsc, xs'') ->
                (loc_stack, warnings) : chunkWarnings (lss : lsc) xs''
        _ -> [(loc_stack, xs)]

  filterWarnings :: [([String], [String])] -> [([String], [String])]
  filterWarnings [] = []
  -- If the warnings are already empty then we are probably doing
  -- something wrong, so don't delete anything
  filterWarnings ((xs, []) : zs) = (xs, []) : filterWarnings zs
  filterWarnings ((xs, ys) : zs) = case filter wantedWarning ys of
                                       [] -> filterWarnings zs
                                       ys' -> (xs, ys') : filterWarnings zs

  unChunkWarnings :: [([String], [String])] -> [String]
  unChunkWarnings [] = []
  unChunkWarnings ((xs, ys) : zs) = xs ++ ys ++ unChunkWarnings zs

  loc_stack_start        s = "In file included from " `isPrefixOf` s
  loc_start_continuation s = "                 from " `isPrefixOf` s
  wantedWarning w
   | "warning: call-clobbered register used" `isContainedIn` w = False
   | otherwise = True

isContainedIn :: String -> String -> Bool
xs `isContainedIn` ys = any (xs `isPrefixOf`) (tails ys)

askCc :: DynFlags -> [Option] -> IO String
askCc dflags args = do
  let (p,args0) = pgm_c dflags
      args1 = map Option (getOpts dflags opt_c)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingWith dflags "gcc" p args2 $ \real_args ->
    readCreateProcess (proc p real_args){ env = mb_env }

-- Version of System.Process.readProcessWithExitCode that takes an environment
readCreateProcess
    :: CreateProcess
    -> IO (ExitCode, String)    -- ^ stdout
readCreateProcess proc = do
    (_, Just outh, _, pid) <-
        createProcess proc{ std_out = CreatePipe }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (length output) >> putMVar outMVar ()

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, output)


-- If the -B<dir> option is set, add <dir> to PATH.  This works around
-- a bug in gcc on Windows Vista where it can't find its auxiliary
-- binaries (see bug #1110).
getGccEnv :: [Option] -> IO (Maybe [(String,String)])
getGccEnv opts =
  if null b_dirs
     then return Nothing
     else do env <- getEnvironment
             return (Just (map mangle_path env))
 where
  (b_dirs, _) = partitionWith get_b_opt opts

  get_b_opt (Option ('-':'B':dir)) = Left dir
  get_b_opt other = Right other

  mangle_path (path,paths) | map toUpper path == "PATH"
        = (path, '\"' : head b_dirs ++ "\";" ++ paths)
  mangle_path other = other

runSplit :: DynFlags -> [Option] -> IO ()
runSplit dflags args = do
  let (p,args0) = pgm_s dflags
  runSomething dflags "Splitter" p (args0++args)

runAs :: DynFlags -> [Option] -> IO ()
runAs dflags args = do
  let (p,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Assembler" p args2 mb_env

-- | Run the LLVM Optimiser
runLlvmOpt :: DynFlags -> [Option] -> IO ()
runLlvmOpt dflags args = do
  let (p,args0) = pgm_lo dflags
      args1 = map Option (getOpts dflags opt_lo)
  runSomething dflags "LLVM Optimiser" p (args0 ++ args1 ++ args)

-- | Run the LLVM Compiler
runLlvmLlc :: DynFlags -> [Option] -> IO ()
runLlvmLlc dflags args = do
  let (p,args0) = pgm_lc dflags
      args1 = map Option (getOpts dflags opt_lc)
  runSomething dflags "LLVM Compiler" p (args0 ++ args1 ++ args)

-- | Run the clang compiler (used as an assembler for the LLVM
-- backend on OS X as LLVM doesn't support the OS X system
-- assembler)
runClang :: DynFlags -> [Option] -> IO ()
runClang dflags args = do
  -- we simply assume its available on the PATH
  let clang = "clang"
      -- be careful what options we call clang with
      -- see #5903 and #7617 for bugs caused by this.
      (_,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  Exception.catch (do
        runSomethingFiltered dflags id "Clang (Assembler)" clang args2 mb_env
    )
    (\(err :: SomeException) -> do
        errorMsg dflags $
            text ("Error running clang! you need clang installed to use the" ++
                "LLVM backend") $+$
            text "(or GHC tried to execute clang incorrectly)"
        throwIO err
    )

-- | Figure out which version of LLVM we are running this session
figureLlvmVersion :: DynFlags -> IO (Maybe Int)
figureLlvmVersion dflags = do
  let (pgm,opts) = pgm_lc dflags
      args = filter notNull (map showOpt opts)
      -- we grab the args even though they should be useless just in
      -- case the user is using a customised 'llc' that requires some
      -- of the options they've specified. llc doesn't care what other
      -- options are specified when '-version' is used.
      args' = args ++ ["-version"]
  ver <- catchIO (do
             (pin, pout, perr, _) <- runInteractiveProcess pgm args'
                                             Nothing Nothing
             {- > llc -version
                  Low Level Virtual Machine (http://llvm.org/):
                    llvm version 2.8 (Ubuntu 2.8-0Ubuntu1)
                    ...
             -}
             hSetBinaryMode pout False
             _     <- hGetLine pout
             vline <- hGetLine pout
             v     <- case filter isDigit vline of
                            []      -> fail "no digits!"
                            [x]     -> fail $ "only 1 digit! (" ++ show x ++ ")"
                            (x:y:_) -> return ((read [x,y]) :: Int)
             hClose pin
             hClose pout
             hClose perr
             return $ Just v
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out LLVM version):" <+>
                     text (show err))
                errorMsg dflags $ vcat
                    [ text "Warning:", nest 9 $
                          text "Couldn't figure out LLVM version!" $$
                          text "Make sure you have installed LLVM"]
                return Nothing)
  return ver

{- Note [Windows stack usage]

See: Trac #8870 (and #8834 for related info)

On Windows, occasionally we need to grow the stack. In order to do
this, we would normally just bump the stack pointer - but there's a
catch on Windows.

If the stack pointer is bumped by more than a single page, then the
pages between the initial pointer and the resulting location must be
properly committed by the Windows virtual memory subsystem. This is
only needed in the event we bump by more than one page (i.e 4097 bytes
or more).

Windows compilers solve this by emitting a call to a special function
called _chkstk, which does this committing of the pages for you.

The reason this was causing a segfault was because due to the fact the
new code generator tends to generate larger functions, we needed more
stack space in GHC itself. In the x86 codegen, we needed approximately
~12kb of stack space in one go, which caused the process to segfault,
as the intervening pages were not committed.

In the future, we should do the same thing, to make the problem
completely go away. In the mean time, we're using a workaround: we
instruct the linker to specify the generated PE as having an initial
reserved stack size of 8mb, as well as a initial *committed* stack
size of 8mb. The default committed size was previously only 4k.

Theoretically it's possible to still hit this problem if you request a
stack bump of more than 8mb in one go. But the amount of code
necessary is quite large, and 8mb "should be more than enough for
anyone" right now (he said, before millions of lines of code cried out
in terror).

-}

{- Note [Run-time linker info]

See also: Trac #5240, Trac #6063

Before 'runLink', we need to be sure to get the relevant information
about the linker we're using at runtime to see if we need any extra
options. For example, GNU ld requires '--reduce-memory-overheads' and
'--hash-size=31' in order to use reasonable amounts of memory (see
trac #5240.) But this isn't supported in GNU gold.

Generally, the linker changing from what was detected at ./configure
time has always been possible using -pgml, but on Linux it can happen
'transparently' by installing packages like binutils-gold, which
change what /usr/bin/ld actually points to.

Clang vs GCC notes:

For gcc, 'gcc -Wl,--version' gives a bunch of output about how to
invoke the linker before the version information string. For 'clang',
the version information for 'ld' is all that's output. For this
reason, we typically need to slurp up all of the standard error output
and look through it.

Other notes:

We cache the LinkerInfo inside DynFlags, since clients may link
multiple times. The definition of LinkerInfo is there to avoid a
circular dependency.

-}


neededLinkArgs :: LinkerInfo -> [Option]
neededLinkArgs (GnuLD o)     = o
neededLinkArgs (GnuGold o)   = o
neededLinkArgs (DarwinLD o)  = o
neededLinkArgs (SolarisLD o) = o
neededLinkArgs UnknownLD     = []

-- Grab linker info and cache it in DynFlags.
getLinkerInfo :: DynFlags -> IO LinkerInfo
getLinkerInfo dflags = do
  info <- readIORef (rtldInfo dflags)
  case info of
    Just v  -> return v
    Nothing -> do
      v <- getLinkerInfo' dflags
      writeIORef (rtldInfo dflags) (Just v)
      return v

-- See Note [Run-time linker info].
getLinkerInfo' :: DynFlags -> IO LinkerInfo
getLinkerInfo' dflags = do
  let platform = targetPlatform dflags
      os = platformOS platform
      (pgm,_) = pgm_l dflags

      -- Try to grab the info from the process output.
      parseLinkerInfo stdo _stde _exitc
        | any ("GNU ld" `isPrefixOf`) stdo =
          -- GNU ld specifically needs to use less memory. This especially
          -- hurts on small object files. Trac #5240.
          return (GnuLD $ map Option ["-Wl,--hash-size=31",
                                      "-Wl,--reduce-memory-overheads"])

        | any ("GNU gold" `isPrefixOf`) stdo =
          -- GNU gold does not require any special arguments.
          return (GnuGold [])

         -- Unknown linker.
        | otherwise = fail "invalid --version output, or linker is unsupported"

  -- Process the executable call
  info <- catchIO (do
             case os of
               OSSolaris2 ->
                 -- Solaris uses its own Solaris linker. Even all
                 -- GNU C are recommended to configure with Solaris
                 -- linker instead of using GNU binutils linker. Also
                 -- all GCC distributed with Solaris follows this rule
                 -- precisely so we assume here, the Solaris linker is
                 -- used.
                 return $ SolarisLD []
               OSDarwin ->
                 -- Darwin has neither GNU Gold or GNU LD, but a strange linker
                 -- that doesn't support --version. We can just assume that's
                 -- what we're using.
                 return $ DarwinLD []
               OSiOS -> 
                 -- Ditto for iOS
                 return $ DarwinLD []
               OSMinGW32 ->
                 -- GHC doesn't support anything but GNU ld on Windows anyway.
                 -- Process creation is also fairly expensive on win32, so
                 -- we short-circuit here.
                 return $ GnuLD $ map Option
                   [ -- Reduce ld memory usage
                     "-Wl,--hash-size=31"
                   , "-Wl,--reduce-memory-overheads"
                     -- Increase default stack, see
                     -- Note [Windows stack usage]
                   , "-Xlinker", "--stack=0x800000,0x800000" ]
               _ -> do
                 -- In practice, we use the compiler as the linker here. Pass
                 -- -Wl,--version to get linker version info.
                 (exitc, stdo, stde) <- readProcessWithExitCode pgm
                                        ["-Wl,--version"] ""
                 -- Split the output by lines to make certain kinds
                 -- of processing easier. In particular, 'clang' and 'gcc'
                 -- have slightly different outputs for '-Wl,--version', but
                 -- it's still easy to figure out.
                 parseLinkerInfo (lines stdo) (lines stde) exitc
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out linker information):" <+>
                     text (show err))
                errorMsg dflags $ hang (text "Warning:") 9 $
                  text "Couldn't figure out linker information!" $$
                  text "Make sure you're using GNU ld, GNU gold" <+>
                  text "or the built in OS X linker, etc."
                return UnknownLD)
  return info

-- Grab compiler info and cache it in DynFlags.
getCompilerInfo :: DynFlags -> IO CompilerInfo
getCompilerInfo dflags = do
  info <- readIORef (rtccInfo dflags)
  case info of
    Just v  -> return v
    Nothing -> do
      v <- getCompilerInfo' dflags
      writeIORef (rtccInfo dflags) (Just v)
      return v

-- See Note [Run-time linker info].
getCompilerInfo' :: DynFlags -> IO CompilerInfo
getCompilerInfo' dflags = do
  let (pgm,_) = pgm_c dflags
      -- Try to grab the info from the process output.
      parseCompilerInfo _stdo stde _exitc
        -- Regular GCC
        | any ("gcc version" `isPrefixOf`) stde =
          return GCC
        -- Regular clang
        | any ("clang version" `isPrefixOf`) stde =
          return Clang
        -- XCode 5 clang
        | any ("Apple LLVM version" `isPrefixOf`) stde =
          return Clang
        -- XCode 4.1 clang
        | any ("Apple clang version" `isPrefixOf`) stde =
          return Clang
         -- Unknown linker.
        | otherwise = fail "invalid -v output, or compiler is unsupported"

  -- Process the executable call
  info <- catchIO (do
                (exitc, stdo, stde) <- readProcessWithExitCode pgm ["-v"] ""
                -- Split the output by lines to make certain kinds
                -- of processing easier.
                parseCompilerInfo (lines stdo) (lines stde) exitc
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out compiler information):" <+>
                     text (show err))
                errorMsg dflags $ hang (text "Warning:") 9 $
                  text "Couldn't figure out linker information!" $$
                  text "Make sure you're using GNU gcc, or clang"
                return UnknownCC)
  return info

runLink :: DynFlags -> [Option] -> IO ()
runLink dflags args = do
  -- See Note [Run-time linker info]
  linkargs <- neededLinkArgs `fmap` getLinkerInfo dflags
  let (p,args0) = pgm_l dflags
      args1     = map Option (getOpts dflags opt_l)
      args2     = args0 ++ args1 ++ args ++ linkargs
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Linker" p args2 mb_env

runLibtool :: DynFlags -> [Option] -> IO ()
runLibtool dflags args = do
  linkargs <- neededLinkArgs `fmap` getLinkerInfo dflags
  let args1      = map Option (getOpts dflags opt_l)
      args2      = [Option "-static"] ++ args1 ++ args ++ linkargs
      libtool    = pgm_libtool dflags    
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Linker" libtool args2 mb_env

runMkDLL :: DynFlags -> [Option] -> IO ()
runMkDLL dflags args = do
  let (p,args0) = pgm_dll dflags
      args1 = args0 ++ args
  mb_env <- getGccEnv (args0++args)
  runSomethingFiltered dflags id "Make DLL" p args1 mb_env

runWindres :: DynFlags -> [Option] -> IO ()
runWindres dflags args = do
  let (gcc, gcc_args) = pgm_c dflags
      windres = pgm_windres dflags
      opts = map Option (getOpts dflags opt_windres)
      quote x = "\"" ++ x ++ "\""
      args' = -- If windres.exe and gcc.exe are in a directory containing
              -- spaces then windres fails to run gcc. We therefore need
              -- to tell it what command to use...
              Option ("--preprocessor=" ++
                      unwords (map quote (gcc :
                                          map showOpt gcc_args ++
                                          map showOpt opts ++
                                          ["-E", "-xc", "-DRC_INVOKED"])))
              -- ...but if we do that then if windres calls popen then
              -- it can't understand the quoting, so we have to use
              -- --use-temp-file so that it interprets it correctly.
              -- See #1828.
            : Option "--use-temp-file"
            : args
  mb_env <- getGccEnv gcc_args
  runSomethingFiltered dflags id "Windres" windres args' mb_env

touch :: DynFlags -> String -> String -> IO ()
touch dflags purpose arg =
  runSomething dflags purpose (pgm_T dflags) [FileOption "" arg]

copy :: DynFlags -> String -> FilePath -> FilePath -> IO ()
copy dflags purpose from to = copyWithHeader dflags purpose Nothing from to

copyWithHeader :: DynFlags -> String -> Maybe String -> FilePath -> FilePath
               -> IO ()
copyWithHeader dflags purpose maybe_header from to = do
  showPass dflags purpose

  hout <- openBinaryFile to   WriteMode
  hin  <- openBinaryFile from ReadMode
  ls <- hGetContents hin -- inefficient, but it'll do for now. ToDo: speed up
  maybe (return ()) (header hout) maybe_header
  hPutStr hout ls
  hClose hout
  hClose hin
 where
  -- write the header string in UTF-8.  The header is something like
  --   {-# LINE "foo.hs" #-}
  -- and we want to make sure a Unicode filename isn't mangled.
  header h str = do
   hSetEncoding h utf8
   hPutStr h str
   hSetBinaryMode h True

-- | read the contents of the named section in an ELF object as a
-- String.
readElfSection :: DynFlags -> String -> FilePath -> IO (Maybe String)
readElfSection _dflags section exe = do
  let
     prog = "readelf"
     args = [Option "-p", Option section, FileOption "" exe]
  --
  r <- readProcessWithExitCode prog (filter notNull (map showOpt args)) ""
  case r of
    (ExitSuccess, out, _err) -> return (doFilter (lines out))
    _ -> return Nothing
 where
  doFilter [] = Nothing
  doFilter (s:r) = case readP_to_S parse s of
                    [(p,"")] -> Just p
                    _r       -> doFilter r
   where parse = do
           skipSpaces
           _ <- R.char '['
           skipSpaces
           _ <- string "0]"
           skipSpaces
           munch (const True)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Managing temporary files
%*                                                                      *
%************************************************************************

\begin{code}
cleanTempDirs :: DynFlags -> IO ()
cleanTempDirs dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = dirsToClean dflags
        ds <- atomicModifyIORef ref $ \ds -> (Map.empty, ds)
        removeTmpDirs dflags (Map.elems ds)

cleanTempFiles :: DynFlags -> IO ()
cleanTempFiles dflags
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = filesToClean dflags
        fs <- atomicModifyIORef ref $ \fs -> ([],fs)
        removeTmpFiles dflags fs

cleanTempFilesExcept :: DynFlags -> [FilePath] -> IO ()
cleanTempFilesExcept dflags dont_delete
   = unless (gopt Opt_KeepTmpFiles dflags)
   $ mask_
   $ do let ref = filesToClean dflags
        to_delete <- atomicModifyIORef ref $ \files ->
            let (to_keep,to_delete) = partition (`elem` dont_delete) files
            in  (to_keep,to_delete)
        removeTmpFiles dflags to_delete


-- Return a unique numeric temp file suffix
newTempSuffix :: DynFlags -> IO Int
newTempSuffix dflags = atomicModifyIORef (nextTempSuffix dflags) $ \n -> (n+1,n)

-- Find a temporary name that doesn't already exist.
newTempName :: DynFlags -> Suffix -> IO FilePath
newTempName dflags extn
  = do d <- getTempDir dflags
       x <- getProcessID
       findTempName (d </> "ghc" ++ show x ++ "_")
  where
    findTempName :: FilePath -> IO FilePath
    findTempName prefix
      = do n <- newTempSuffix dflags
           let filename = prefix ++ show n <.> extn
           b <- doesFileExist filename
           if b then findTempName prefix
                else do -- clean it up later
                        consIORef (filesToClean dflags) filename
                        return filename

-- Return our temporary directory within tmp_dir, creating one if we
-- don't have one yet.
getTempDir :: DynFlags -> IO FilePath
getTempDir dflags = do
    mapping <- readIORef dir_ref
    case Map.lookup tmp_dir mapping of
        Nothing -> do
            pid <- getProcessID
            let prefix = tmp_dir </> "ghc" ++ show pid ++ "_"
            mask_ $ mkTempDir prefix
        Just dir -> return dir
  where
    tmp_dir = tmpDir dflags
    dir_ref = dirsToClean dflags

    mkTempDir :: FilePath -> IO FilePath
    mkTempDir prefix = do
        n <- newTempSuffix dflags
        let our_dir = prefix ++ show n

        -- 1. Speculatively create our new directory.
        createDirectory our_dir

        -- 2. Update the dirsToClean mapping unless an entry already exists
        -- (i.e. unless another thread beat us to it).
        their_dir <- atomicModifyIORef dir_ref $ \mapping ->
            case Map.lookup tmp_dir mapping of
                Just dir -> (mapping, Just dir)
                Nothing  -> (Map.insert tmp_dir our_dir mapping, Nothing)

        -- 3. If there was an existing entry, return it and delete the
        -- directory we created.  Otherwise return the directory we created.
        case their_dir of
            Nothing  -> do
                debugTraceMsg dflags 2 $
                    text "Created temporary directory:" <+> text our_dir
                return our_dir
            Just dir -> do
                removeDirectory our_dir
                return dir
      `catchIO` \e -> if isAlreadyExistsError e
                      then mkTempDir prefix else ioError e

addFilesToClean :: DynFlags -> [FilePath] -> IO ()
-- May include wildcards [used by DriverPipeline.run_phase SplitMangle]
addFilesToClean dflags new_files
    = atomicModifyIORef (filesToClean dflags) $ \files -> (new_files++files, ())

removeTmpDirs :: DynFlags -> [FilePath] -> IO ()
removeTmpDirs dflags ds
  = traceCmd dflags "Deleting temp dirs"
             ("Deleting: " ++ unwords ds)
             (mapM_ (removeWith dflags removeDirectory) ds)

removeTmpFiles :: DynFlags -> [FilePath] -> IO ()
removeTmpFiles dflags fs
  = warnNon $
    traceCmd dflags "Deleting temp files"
             ("Deleting: " ++ unwords deletees)
             (mapM_ (removeWith dflags removeFile) deletees)
  where
     -- Flat out refuse to delete files that are likely to be source input
     -- files (is there a worse bug than having a compiler delete your source
     -- files?)
     --
     -- Deleting source files is a sign of a bug elsewhere, so prominently flag
     -- the condition.
    warnNon act
     | null non_deletees = act
     | otherwise         = do
        putMsg dflags (text "WARNING - NOT deleting source files:" <+> hsep (map text non_deletees))
        act

    (non_deletees, deletees) = partition isHaskellUserSrcFilename fs

removeWith :: DynFlags -> (FilePath -> IO ()) -> FilePath -> IO ()
removeWith dflags remover f = remover f `catchIO`
  (\e ->
   let msg = if isDoesNotExistError e
             then ptext (sLit "Warning: deleting non-existent") <+> text f
             else ptext (sLit "Warning: exception raised when deleting")
                                            <+> text f <> colon
               $$ text (show e)
   in debugTraceMsg dflags 2 msg
  )

-----------------------------------------------------------------------------
-- Running an external program

runSomething :: DynFlags
             -> String          -- For -v message
             -> String          -- Command name (possibly a full path)
                                --      assumed already dos-ified
             -> [Option]        -- Arguments
                                --      runSomething will dos-ify them
             -> IO ()

runSomething dflags phase_name pgm args =
  runSomethingFiltered dflags id phase_name pgm args Nothing

runSomethingFiltered
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO ()

runSomethingFiltered dflags filter_fn phase_name pgm args mb_env = do
    runSomethingWith dflags phase_name pgm args $ \real_args -> do
        r <- builderMainLoop dflags filter_fn pgm real_args mb_env
        return (r,())

runSomethingWith
  :: DynFlags -> String -> String -> [Option]
  -> ([String] -> IO (ExitCode, a))
  -> IO a

runSomethingWith dflags phase_name pgm args io = do
  let real_args = filter notNull (map showOpt args)
      cmdLine = showCommandForUser pgm real_args
  traceCmd dflags phase_name cmdLine $ handleProc pgm phase_name $ io real_args

handleProc :: String -> String -> IO (ExitCode, r) -> IO r
handleProc pgm phase_name proc = do
    (rc, r) <- proc `catchIO` handler
    case rc of
      ExitSuccess{} -> return r
      ExitFailure n
        -- rawSystem returns (ExitFailure 127) if the exec failed for any
        -- reason (eg. the program doesn't exist).  This is the only clue
        -- we have, but we need to report something to the user because in
        -- the case of a missing program there will otherwise be no output
        -- at all.
       | n == 127  -> does_not_exist
       | otherwise -> throwGhcExceptionIO (PhaseFailed phase_name rc)
  where
    handler err =
       if IO.isDoesNotExistError err
          then does_not_exist
          else IO.ioError err

    does_not_exist = throwGhcExceptionIO (InstallationError ("could not execute: " ++ pgm))


builderMainLoop :: DynFlags -> (String -> String) -> FilePath
                -> [String] -> Maybe [(String, String)]
                -> IO ExitCode
builderMainLoop dflags filter_fn pgm real_args mb_env = do
  chan <- newChan
  (hStdIn, hStdOut, hStdErr, hProcess) <- runInteractiveProcess pgm real_args Nothing mb_env

  -- and run a loop piping the output from the compiler to the log_action in DynFlags
  hSetBuffering hStdOut LineBuffering
  hSetBuffering hStdErr LineBuffering
  _ <- forkIO (readerProc chan hStdOut filter_fn)
  _ <- forkIO (readerProc chan hStdErr filter_fn)
  -- we don't want to finish until 2 streams have been completed
  -- (stdout and stderr)
  -- nor until 1 exit code has been retrieved.
  rc <- loop chan hProcess (2::Integer) (1::Integer) ExitSuccess
  -- after that, we're done here.
  hClose hStdIn
  hClose hStdOut
  hClose hStdErr
  return rc
  where
    -- status starts at zero, and increments each time either
    -- a reader process gets EOF, or the build proc exits.  We wait
    -- for all of these to happen (status==3).
    -- ToDo: we should really have a contingency plan in case any of
    -- the threads dies, such as a timeout.
    loop _    _        0 0 exitcode = return exitcode
    loop chan hProcess t p exitcode = do
      mb_code <- if p > 0
                   then getProcessExitCode hProcess
                   else return Nothing
      case mb_code of
        Just code -> loop chan hProcess t (p-1) code
        Nothing
          | t > 0 -> do
              msg <- readChan chan
              case msg of
                BuildMsg msg -> do
                  log_action dflags dflags SevInfo noSrcSpan defaultUserStyle msg
                  loop chan hProcess t p exitcode
                BuildError loc msg -> do
                  log_action dflags dflags SevError (mkSrcSpan loc loc) defaultUserStyle msg
                  loop chan hProcess t p exitcode
                EOF ->
                  loop chan hProcess (t-1) p exitcode
          | otherwise -> loop chan hProcess t p exitcode

readerProc :: Chan BuildMessage -> Handle -> (String -> String) -> IO ()
readerProc chan hdl filter_fn =
    (do str <- hGetContents hdl
        loop (linesPlatform (filter_fn str)) Nothing)
    `finally`
       writeChan chan EOF
        -- ToDo: check errors more carefully
        -- ToDo: in the future, the filter should be implemented as
        -- a stream transformer.
    where
        loop []     Nothing    = return ()
        loop []     (Just err) = writeChan chan err
        loop (l:ls) in_err     =
                case in_err of
                  Just err@(BuildError srcLoc msg)
                    | leading_whitespace l -> do
                        loop ls (Just (BuildError srcLoc (msg $$ text l)))
                    | otherwise -> do
                        writeChan chan err
                        checkError l ls
                  Nothing -> do
                        checkError l ls
                  _ -> panic "readerProc/loop"

        checkError l ls
           = case parseError l of
                Nothing -> do
                    writeChan chan (BuildMsg (text l))
                    loop ls Nothing
                Just (file, lineNum, colNum, msg) -> do
                    let srcLoc = mkSrcLoc (mkFastString file) lineNum colNum
                    loop ls (Just (BuildError srcLoc (text msg)))

        leading_whitespace []    = False
        leading_whitespace (x:_) = isSpace x

parseError :: String -> Maybe (String, Int, Int, String)
parseError s0 = case breakColon s0 of
                Just (filename, s1) ->
                    case breakIntColon s1 of
                    Just (lineNum, s2) ->
                        case breakIntColon s2 of
                        Just (columnNum, s3) ->
                            Just (filename, lineNum, columnNum, s3)
                        Nothing ->
                            Just (filename, lineNum, 0, s2)
                    Nothing -> Nothing
                Nothing -> Nothing

breakColon :: String -> Maybe (String, String)
breakColon xs = case break (':' ==) xs of
                    (ys, _:zs) -> Just (ys, zs)
                    _ -> Nothing

breakIntColon :: String -> Maybe (Int, String)
breakIntColon xs = case break (':' ==) xs of
                       (ys, _:zs)
                        | not (null ys) && all isAscii ys && all isDigit ys ->
                           Just (read ys, zs)
                       _ -> Nothing

data BuildMessage
  = BuildMsg   !SDoc
  | BuildError !SrcLoc !SDoc
  | EOF

traceCmd :: DynFlags -> String -> String -> IO a -> IO a
-- trace the command (at two levels of verbosity)
traceCmd dflags phase_name cmd_line action
 = do   { let verb = verbosity dflags
        ; showPass dflags phase_name
        ; debugTraceMsg dflags 3 (text cmd_line)
        ; case flushErr dflags of
              FlushErr io -> io

           -- And run it!
        ; action `catchIO` handle_exn verb
        }
  where
    handle_exn _verb exn = do { debugTraceMsg dflags 2 (char '\n')
                              ; debugTraceMsg dflags 2 (ptext (sLit "Failed:") <+> text cmd_line <+> text (show exn))
                              ; throwGhcExceptionIO (PhaseFailed phase_name (ExitFailure 1)) }
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Support code}
%*                                                                      *
%************************************************************************

\begin{code}
-----------------------------------------------------------------------------
-- Define       getBaseDir     :: IO (Maybe String)

getBaseDir :: IO (Maybe String)
#if defined(mingw32_HOST_OS)
-- Assuming we are running ghc, accessed by path  $(stuff)/bin/ghc.exe,
-- return the path $(stuff)/lib.
getBaseDir = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> fmap (Just . rootDir) $ peekCWString buf
            | otherwise  -> try_size (size * 2)
    
    rootDir s = case splitFileName $ normalise s of
                (d, ghc_exe)
                 | lower ghc_exe `elem` ["ghc.exe",
                                         "ghc-stage1.exe",
                                         "ghc-stage2.exe",
                                         "ghc-stage3.exe"] ->
                    case splitFileName $ takeDirectory d of
                    -- ghc is in $topdir/bin/ghc.exe
                    (d', bin) | lower bin == "bin" -> takeDirectory d' </> "lib"
                    _ -> fail
                _ -> fail
        where fail = panic ("can't decompose ghc.exe path: " ++ show s)
              lower = map toLower

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getBaseDir = return Nothing
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int -- relies on Int == Int32 on Windows
#else
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#endif

-- Divvy up text stream into lines, taking platform dependent
-- line termination into account.
linesPlatform :: String -> [String]
#if !defined(mingw32_HOST_OS)
linesPlatform ls = lines ls
#else
linesPlatform "" = []
linesPlatform xs =
  case lineBreak xs of
    (as,xs1) -> as : linesPlatform xs1
  where
   lineBreak "" = ("","")
   lineBreak ('\r':'\n':xs) = ([],xs)
   lineBreak ('\n':xs) = ([],xs)
   lineBreak (x:xs) = let (as,bs) = lineBreak xs in (x:as,bs)

#endif

linkDynLib :: DynFlags -> [String] -> [PackageId] -> IO ()
linkDynLib dflags0 o_files dep_packages
 = do
    let -- This is a rather ugly hack to fix dynamically linked
        -- GHC on Windows. If GHC is linked with -threaded, then
        -- it links against libHSrts_thr. But if base is linked
        -- against libHSrts, then both end up getting loaded,
        -- and things go wrong. We therefore link the libraries
        -- with the same RTS flags that we link GHC with.
        dflags1 = if cGhcThreaded then addWay' WayThreaded dflags0
                                  else                     dflags0
        dflags2 = if cGhcDebugged then addWay' WayDebug dflags1
                                  else                  dflags1
        dflags = updateWays dflags2

        verbFlags = getVerbFlags dflags
        o_file = outputFile dflags

    pkgs <- getPreloadPackagesAnd dflags dep_packages

    let pkg_lib_paths = collectLibraryPaths pkgs
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | ( osElfTarget (platformOS (targetPlatform dflags)) ||
             osMachOTarget (platformOS (targetPlatform dflags)) ) &&
           dynLibLoader dflags == SystemDependent &&
           not (gopt Opt_Static dflags)
            = ["-L" ++ l, "-Wl,-rpath", "-Wl," ++ l]
         | otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    -- We don't want to link our dynamic libs against the RTS package,
    -- because the RTS lib comes in several flavours and we want to be
    -- able to pick the flavour when a binary is linked.
    -- On Windows we need to link the RTS import lib as Windows does
    -- not allow undefined symbols.
    -- The RTS library path is still added to the library search path
    -- above in case the RTS is being explicitly linked in (see #3807).
    let platform = targetPlatform dflags
        os = platformOS platform
        pkgs_no_rts = case os of
                      OSMinGW32 ->
                          pkgs
                      _ ->
                          filter ((/= rtsPackageId) . packageConfigId) pkgs
    let pkg_link_opts = let (package_hs_libs, extra_libs, other_flags) = collectLinkOpts dflags pkgs_no_rts
                        in  package_hs_libs ++ extra_libs ++ other_flags

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    case os of
        OSMinGW32 -> do
            -------------------------------------------------------------
            -- Making a DLL
            -------------------------------------------------------------
            let output_fn = case o_file of
                            Just s -> s
                            Nothing -> "HSdll.dll"

            runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    , Option "-shared"
                    ] ++
                    [ FileOption "-Wl,--out-implib=" (output_fn ++ ".a")
                    | gopt Opt_SharedImplib dflags
                    ]
                 ++ map (FileOption "") o_files

                 -- Permit the linker to auto link _symbol to _imp_symbol
                 -- This lets us link against DLLs without needing an "import library"
                 ++ [Option "-Wl,--enable-auto-import"]

                 ++ extra_ld_inputs
                 ++ map Option (
                    lib_path_opts
                 ++ pkg_lib_path_opts
                 ++ pkg_link_opts
                ))
        OSDarwin -> do
            -------------------------------------------------------------------
            -- Making a darwin dylib
            -------------------------------------------------------------------
            -- About the options used for Darwin:
            -- -dynamiclib
            --   Apple's way of saying -shared
            -- -undefined dynamic_lookup:
            --   Without these options, we'd have to specify the correct
            --   dependencies for each of the dylibs. Note that we could
            --   (and should) do without this for all libraries except
            --   the RTS; all we need to do is to pass the correct
            --   HSfoo_dyn.dylib files to the link command.
            --   This feature requires Mac OS X 10.3 or later; there is
            --   a similar feature, -flat_namespace -undefined suppress,
            --   which works on earlier versions, but it has other
            --   disadvantages.
            -- -single_module
            --   Build the dynamic library as a single "module", i.e. no
            --   dynamic binding nonsense when referring to symbols from
            --   within the library. The NCG assumes that this option is
            --   specified (on i386, at least).
            -- -install_name
            --   Mac OS/X stores the path where a dynamic library is (to
            --   be) installed in the library itself.  It's called the
            --   "install name" of the library. Then any library or
            --   executable that links against it before it's installed
            --   will search for it in its ultimate install location.
            --   By default we set the install name to the absolute path
            --   at build time, but it can be overridden by the
            --   -dylib-install-name option passed to ghc. Cabal does
            --   this.
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }

            instName <- case dylibInstallName dflags of
                Just n -> return n
                Nothing -> return $ "@rpath" `combine` (takeFileName output_fn)
            runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-dynamiclib"
                    , Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-undefined",
                      Option "dynamic_lookup",
                      Option "-single_module" ]
                 ++ (if platformArch platform == ArchX86_64
                     then [ ]
                     else [ Option "-Wl,-read_only_relocs,suppress" ])
                 ++ [ Option "-install_name", Option instName ]
                 ++ map Option lib_path_opts
                 ++ extra_ld_inputs
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
              )
        OSiOS -> throwGhcExceptionIO (ProgramError "dynamic libraries are not supported on iOS target")
        _ -> do
            -------------------------------------------------------------------
            -- Making a DSO
            -------------------------------------------------------------------

            let output_fn = case o_file of { Just s -> s; Nothing -> "a.out"; }
            let buildingRts = thisPackage dflags == rtsPackageId
            let bsymbolicFlag = if buildingRts
                                then -- -Bsymbolic breaks the way we implement
                                     -- hooks in the RTS
                                     []
                                else -- we need symbolic linking to resolve
                                     -- non-PIC intra-package-relocations
                                     ["-Wl,-Bsymbolic"]

            runLink dflags (
                    map Option verbFlags
                 ++ [ Option "-o"
                    , FileOption "" output_fn
                    ]
                 ++ map Option o_files
                 ++ [ Option "-shared" ]
                 ++ map Option bsymbolicFlag
                    -- Set the library soname. We use -h rather than -soname as
                    -- Solaris 10 doesn't support the latter:
                 ++ [ Option ("-Wl,-h," ++ takeFileName output_fn) ]
                 ++ map Option lib_path_opts
                 ++ extra_ld_inputs
                 ++ map Option pkg_lib_path_opts
                 ++ map Option pkg_link_opts
              )
\end{code}
