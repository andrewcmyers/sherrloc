{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module System.Exit (
      ExitCode(ExitSuccess,ExitFailure)
    , exitWith      -- :: ExitCode -> IO a
    , exitFailure   -- :: IO a
    , exitSuccess   -- :: IO a
  ) where
import "base" System.Exit hiding (exitWith)
import qualified "base" System.Exit as Base

-- SDM: use the Haskell 98 docs for exitWith, since the base docs talk
-- about exceptions which aren't in Haskell 2010.

-- SDM: removed:
-- Before the program terminates, any open or semi-closed handles are
-- first closed.

-- SDM: removed:
-- If a program terminates as a result of calling @error@\indextt{error} or
-- because its value is otherwise determined to be "\bot"\index{"\bot"}, then it
-- is treated identically to the computation @exitFailure@.  Otherwise, if any
-- program "p" terminates without calling @exitWith@ explicitly, it is treated
-- identically to the computation
-- \bprog
-- @(@"p"@ >> exitWith ExitSuccess) `catch` \ _ -> exitFailure@
-- \eprog

{- |
Computation @'exitWith' code@ terminates the program, returning @code@
to the program's caller.
The caller may interpret the return code as it wishes, but the program
should return 'ExitSuccess' to mean normal completion, and
@'ExitFailure' n@ to mean that the program encountered a problem from
which it could not recover.  The value 'exitFailure' is equal to
@'exitWith' ('ExitFailure' exitfail)@, where @exitfail@ is
implementation-dependent.  'exitWith' bypasses the error handling in
the I/O monad and cannot be intercepted by 'catch' from the @Prelude@.
-}
exitWith :: ExitCode -> IO a
exitWith = Base.exitWith
