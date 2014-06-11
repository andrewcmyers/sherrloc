{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module System.IO (
    -- * The IO monad

    IO,                        -- instance MonadFix
    fixIO,                     -- :: (a -> IO a) -> IO a

    -- * Files and handles

    FilePath,                  -- :: String

    Handle,             -- abstract, instance of: Eq, Show.

    -- ** Standard handles

    -- | Three handles are allocated during program initialisation,
    -- and are initially open.

    stdin, stdout, stderr,   -- :: Handle

    -- * Opening and closing files

    -- ** Opening files

    withFile,
    openFile,                  -- :: FilePath -> IOMode -> IO Handle
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),

    -- ** Closing files

    hClose,                    -- :: Handle -> IO ()

    -- ** Special cases

    -- | These functions are also exported by the "Prelude".

    readFile,                  -- :: FilePath -> IO String
    writeFile,                 -- :: FilePath -> String -> IO ()
    appendFile,                -- :: FilePath -> String -> IO ()

    -- ** File locking

    -- $locking

    -- * Operations on handles

    -- ** Determining and changing the size of a file

    hFileSize,                 -- :: Handle -> IO Integer
    hSetFileSize,              -- :: Handle -> Integer -> IO ()

    -- ** Detecting the end of input

    hIsEOF,                    -- :: Handle -> IO Bool
    isEOF,                     -- :: IO Bool

    -- ** Buffering operations

    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    hSetBuffering,             -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,             -- :: Handle -> IO BufferMode
    hFlush,                    -- :: Handle -> IO ()

    -- ** Repositioning handles

    hGetPosn,                  -- :: Handle -> IO HandlePosn
    hSetPosn,                  -- :: HandlePosn -> IO ()
    HandlePosn,                -- abstract, instance of: Eq, Show.

    hSeek,                     -- :: Handle -> SeekMode -> Integer -> IO ()
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
    hTell,                     -- :: Handle -> IO Integer

    -- ** Handle properties

    -- | Each of these operations returns 'True' if the handle has the
    -- the specified property, or 'False' otherwise.

    hIsOpen, hIsClosed,        -- :: Handle -> IO Bool
    hIsReadable, hIsWritable,  -- :: Handle -> IO Bool
    hIsSeekable,               -- :: Handle -> IO Bool

    -- ** Terminal operations

    hIsTerminalDevice,          -- :: Handle -> IO Bool

    hSetEcho,                   -- :: Handle -> Bool -> IO ()
    hGetEcho,                   -- :: Handle -> IO Bool

    -- ** Showing handle state
    hShow,                      -- :: Handle -> IO String

    -- * Text input and output

    -- ** Text input

    hWaitForInput,             -- :: Handle -> Int -> IO Bool
    hReady,                    -- :: Handle -> IO Bool
    hGetChar,                  -- :: Handle -> IO Char
    hGetLine,                  -- :: Handle -> IO [Char]
    hLookAhead,                -- :: Handle -> IO Char
    hGetContents,              -- :: Handle -> IO [Char]

    -- ** Text output

    hPutChar,                  -- :: Handle -> Char -> IO ()
    hPutStr,                   -- :: Handle -> [Char] -> IO ()
    hPutStrLn,                 -- :: Handle -> [Char] -> IO ()
    hPrint,                    -- :: Show a => Handle -> a -> IO ()

    -- ** Special cases for standard input and output

    -- | These functions are also exported by the "Prelude".

    interact,                  -- :: (String -> String) -> IO ()
    putChar,                   -- :: Char   -> IO ()
    putStr,                    -- :: String -> IO ()
    putStrLn,                  -- :: String -> IO ()
    print,                     -- :: Show a => a -> IO ()
    getChar,                   -- :: IO Char
    getLine,                   -- :: IO String
    getContents,               -- :: IO String
    readIO,                    -- :: Read a => String -> IO a
    readLn,                    -- :: Read a => IO a

  ) where

import "base" System.IO hiding (openFile, hWaitForInput)
import qualified "base" System.IO as Base

-- $locking
-- Implementations should enforce as far as possible, at least locally to the
-- Haskell process, multiple-reader single-writer locking on files.
-- That is, /there may either be many handles on the same file which manage input, or just one handle on the file which manages output/.  If any
-- open or semi-closed handle is managing a file for output, no new
-- handle can be allocated for that file.  If any open or semi-closed
-- handle is managing a file for input, new handles can only be allocated
-- if they do not manage output.  Whether two files are the same is
-- implementation-dependent, but they should normally be the same if they
-- have the same absolute path name and neither has been renamed, for
-- example.
--
-- /Warning/: the 'readFile' operation holds a semi-closed handle on
-- the file until the entire contents of the file have been consumed.
-- It follows that an attempt to write to a file (using 'writeFile', for
-- example) that was earlier opened by 'readFile' will usually result in
-- failure with 'System.IO.Error.isAlreadyInUseError'.


-- SDM: custom verison of openFile docs removing reference to 'openBinaryFile'

-- | Computation 'openFile' @file mode@ allocates and returns a new, open
-- handle to manage the file @file@.  It manages input if @mode@
-- is 'ReadMode', output if @mode@ is 'WriteMode' or 'AppendMode',
-- and both input and output if mode is 'ReadWriteMode'.
--
-- If the file does not exist and it is opened for output, it should be
-- created as a new file.  If @mode@ is 'WriteMode' and the file
-- already exists, then it should be truncated to zero length.
-- Some operating systems delete empty files, so there is no guarantee
-- that the file will exist following an 'openFile' with @mode@
-- 'WriteMode' unless it is subsequently written to successfully.
-- The handle is positioned at the end of the file if @mode@ is
-- 'AppendMode', and otherwise at the beginning (in which case its
-- internal position is 0).
-- The initial buffer mode is implementation-dependent.
--
-- This operation may fail with:
--
--  * 'isAlreadyInUseError' if the file is already open and cannot be reopened;
--
--  * 'isDoesNotExistError' if the file does not exist; or
--
--  * 'isPermissionError' if the user does not have permission to open the file.
--
openFile :: FilePath -> IOMode -> IO Handle
openFile = Base.openFile

-- SDM: local version of docs for hWaitForInput, omitting GHC-specific notes.

-- If hWaitForInput finds anything in the Handle's buffer, it
-- immediately returns.  If not, it tries to read from the underlying
-- OS handle. Notice that for buffered Handles connected to terminals
-- this means waiting until a complete line is available.

-- | Computation 'hWaitForInput' @hdl t@
-- waits until input is available on handle @hdl@.
-- It returns 'True' as soon as input is available on @hdl@,
-- or 'False' if no input is available within @t@ milliseconds.  Note that
-- 'hWaitForInput' waits until one or more full /characters/ are available,
-- which means that it needs to do decoding, and hence may fail
-- with a decoding error.
--
-- If @t@ is less than zero, then @hWaitForInput@ waits indefinitely.
--
-- This operation may fail with:
--
--  * 'isEOFError' if the end of file has been reached.
--
--  * a decoding error, if the input begins with an invalid byte sequence
--    in this Handle's encoding.
--

hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput = Base.hWaitForInput
