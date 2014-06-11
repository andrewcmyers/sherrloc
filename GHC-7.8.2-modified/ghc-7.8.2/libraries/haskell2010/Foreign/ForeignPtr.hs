{-# LANGUAGE PackageImports #-}

module Foreign.ForeignPtr (
        -- * Finalised data pointers
          ForeignPtr
        , FinalizerPtr
        , FinalizerEnvPtr

        -- ** Basic operations
        , newForeignPtr
        , newForeignPtr_
        , addForeignPtrFinalizer
        , newForeignPtrEnv
        , addForeignPtrFinalizerEnv
        , withForeignPtr
        , finalizeForeignPtr

        -- ** Low-level operations
        , unsafeForeignPtrToPtr
        , touchForeignPtr
        , castForeignPtr

        -- ** Allocating managed memory
        , mallocForeignPtr
        , mallocForeignPtrBytes
        , mallocForeignPtrArray
        , mallocForeignPtrArray0
  ) where

import qualified "base" Foreign.ForeignPtr as Base
import "base" Foreign.ForeignPtr hiding (mallocForeignPtr, touchForeignPtr)
import "base" Foreign.ForeignPtr.Unsafe
import "base" Foreign (Storable)

-- SDM: local copy of the docs for mallocForeignPtr, to omit the
-- GHC-specific bits.

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
-- ^ Allocate some memory and return a 'ForeignPtr' to it.  The memory
-- will be released automatically when the 'ForeignPtr' is discarded.
--
-- 'mallocForeignPtr' is equivalent to
--
-- >    do { p <- malloc; newForeignPtr finalizerFree p }
--
-- although it may be implemented differently internally: you may not
-- assume that the memory returned by 'mallocForeignPtr' has been
-- allocated with 'Foreign.Marshal.Alloc.malloc'.
mallocForeignPtr = Base.mallocForeignPtr

-- SDM: reproduced documentation for touchForeignPtr without reference
-- to Haskell finalizers and MVars.

touchForeignPtr :: ForeignPtr a -> IO ()
-- ^This function ensures that the foreign object in
-- question is alive at the given place in the sequence of IO
-- actions. In particular 'Foreign.ForeignPtr.withForeignPtr'
-- does a 'touchForeignPtr' after it
-- executes the user action.
--
-- Note that this function should not be used to express dependencies
-- between finalizers on 'ForeignPtr's.  For example, if the finalizer
-- for a 'ForeignPtr' @F1@ calls 'touchForeignPtr' on a second
-- 'ForeignPtr' @F2@, then the only guarantee is that the finalizer
-- for @F2@ is never started before the finalizer for @F1@.  They
-- might be started together if for example both @F1@ and @F2@ are
-- otherwise unreachable.
--
-- In general, it is not recommended to use finalizers on separate
-- objects with ordering constraints between them.  To express the
-- ordering robustly requires explicit synchronisation between
-- finalizers.
--
touchForeignPtr = Base.touchForeignPtr
