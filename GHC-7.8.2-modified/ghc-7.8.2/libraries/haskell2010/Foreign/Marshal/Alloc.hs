{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

{- |
The module "Foreign.Marshal.Alloc" provides operations to allocate and
deallocate blocks of raw memory (i.e., unstructured chunks of memory
outside of the area maintained by the Haskell storage manager).  These
memory blocks are commonly used to pass compound data structures to
foreign functions or to provide space in which compound result values
are obtained from foreign functions.

If any of the allocation functions fails, a value of 'nullPtr' is
produced.  If 'free' or 'reallocBytes' is applied to a memory area
that has been allocated with 'alloca' or 'allocaBytes', the
behaviour is undefined.  Any further access to memory areas allocated with
'alloca' or 'allocaBytes', after the computation that was passed to
the allocation function has terminated, leads to undefined behaviour.  Any
further access to the memory area referenced by a pointer passed to
'realloc', 'reallocBytes', or 'free' entails undefined
behaviour.

All storage allocated by functions that allocate based on a /size in bytes/
must be sufficiently aligned for any of the basic foreign types
that fits into the newly allocated storage. All storage allocated by
functions that allocate based on a specific type must be sufficiently
aligned for that type. Array allocation routines need to obey the same
alignment constraints for each array element.
-}
module Foreign.Marshal.Alloc (
  -- * Memory allocation
  -- ** Local allocation
  alloca,       -- :: Storable a =>        (Ptr a -> IO b) -> IO b
  allocaBytes,  -- ::               Int -> (Ptr a -> IO b) -> IO b

  -- ** Dynamic allocation
  malloc,       -- :: Storable a =>        IO (Ptr a)
  mallocBytes,  -- ::               Int -> IO (Ptr a)

  realloc,      -- :: Storable b => Ptr a        -> IO (Ptr b)
  reallocBytes, -- ::               Ptr a -> Int -> IO (Ptr a)

  free,         -- :: Ptr a -> IO ()
  finalizerFree -- :: FinalizerPtr a
  ) where
import "base" Foreign.Marshal.Alloc
