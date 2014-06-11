{-# LANGUAGE PackageImports #-}

module Foreign.Marshal (
         -- | The module "Foreign.Marshal" re-exports the other modules in the
         -- @Foreign.Marshal@ hierarchy:

         module Foreign.Marshal.Alloc,
         module Foreign.Marshal.Array,
         module Foreign.Marshal.Error,
        -- Not in Haskell 2010:
        -- , module Foreign.Marshal.Pool
         module Foreign.Marshal.Utils,
         -- | and provides one function:
         unsafeLocalState
  ) where

import "this" Foreign.Marshal.Alloc
import "this" Foreign.Marshal.Array
import "this" Foreign.Marshal.Error
-- Not in Haskell 2010:
-- import "this" Foreign.Marshal.Pool
import "this" Foreign.Marshal.Utils

import "base" System.IO.Unsafe

{- |
Sometimes an external entity is a pure function, except that it passes
arguments and/or results via pointers.  The function
@unsafeLocalState@ permits the packaging of such entities as pure
functions.

The only IO operations allowed in the IO action passed to
@unsafeLocalState@ are (a) local allocation (@alloca@, @allocaBytes@
and derived operations such as @withArray@ and @withCString@), and (b)
pointer operations (@Foreign.Storable@ and @Foreign.Ptr@) on the
pointers to local storage, and (c) foreign functions whose only
observable effect is to read and/or write the locally allocated
memory.  Passing an IO operation that does not obey these rules
results in undefined behaviour.

It is expected that this operation will be
replaced in a future revision of Haskell.
-}
unsafeLocalState :: IO a -> a
unsafeLocalState = unsafePerformIO
