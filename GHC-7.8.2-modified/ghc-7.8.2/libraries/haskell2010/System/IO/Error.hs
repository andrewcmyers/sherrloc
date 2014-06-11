{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module System.IO.Error (
      -- * I\/O errors
    IOError,                    -- = IOException

    userError,                  -- :: String  -> IOError

    mkIOError,                  -- :: IOErrorType -> String -> Maybe Handle
                                --    -> Maybe FilePath -> IOError

    annotateIOError,            -- :: IOError -> String -> Maybe Handle
                                --    -> Maybe FilePath -> IOError

    -- ** Classifying I\/O errors
    isAlreadyExistsError,       -- :: IOError -> Bool
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError,
    isEOFError,
    isIllegalOperation,
    isPermissionError,
    isUserError,

    -- ** Attributes of I\/O errors
    ioeGetErrorString,          -- :: IOError -> String
    ioeGetHandle,               -- :: IOError -> Maybe Handle
    ioeGetFileName,             -- :: IOError -> Maybe FilePath

    -- * Types of I\/O error
    IOErrorType,                -- abstract

    alreadyExistsErrorType,     -- :: IOErrorType
    doesNotExistErrorType,
    alreadyInUseErrorType,
    fullErrorType,
    eofErrorType,
    illegalOperationErrorType,
    permissionErrorType,
    userErrorType,

    -- * Throwing and catching I\/O errors

    ioError,                    -- :: IOError -> IO a

    catch,                      -- :: IO a -> (IOError -> IO a) -> IO a
    try                         -- :: IO a -> IO (Either IOError a)

  ) where

import qualified "base" Control.Exception as Exception
import "base" System.IO.Error hiding (IOError)
import qualified "base" System.IO.Error as Base
import Prelude hiding (IOError,catch)

-- | Errors of type 'IOError' are used by the 'IO' monad.  This is an
-- abstract type; the module "System.IO.Error" provides functions to
-- interrogate and construct values of type 'IOError'.
type IOError = Base.IOError

-- SDM: duplicated docs for catch and try, omitting the part about non-IO
-- exceptions.

-- | The 'catch' function establishes a handler that receives any 'IOError'
-- raised in the action protected by 'catch'.  An 'IOError' is caught by
-- the most recent handler established by 'catch'.  These handlers are
-- not selective: all 'IOError's are caught.  Exception propagation
-- must be explicitly provided in a handler by re-raising any unwanted
-- exceptions.  For example, in
--
-- > f = catch g (\e -> if IO.isEOFError e then return [] else ioError e)
--
-- the function @f@ returns @[]@ when an end-of-file exception
-- (cf. 'System.IO.Error.isEOFError') occurs in @g@; otherwise, the
-- exception is propagated to the next outer handler.
--
-- When an exception propagates outside the main program, the Haskell
-- system prints the associated 'IOError' value and exits the program.
--
catch :: IO a -> (IOError -> IO a) -> IO a
catch = Exception.catch

-- | The construct 'try' @comp@ exposes IO errors which occur within a
-- computation, and which are not fully handled.
--
try            :: IO a -> IO (Either IOError a)
try = Exception.try
