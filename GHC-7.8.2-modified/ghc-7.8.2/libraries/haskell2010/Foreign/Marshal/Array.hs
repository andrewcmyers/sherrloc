{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

{- |
The module "Foreign.Marshal.Array" provides operations for marshalling Haskell
lists into monolithic arrays and vice versa.  Most functions come in two
flavours: one for arrays terminated by a special termination element and one
where an explicit length parameter is used to determine the extent of an
array.  The typical example for the former case are C's NUL terminated
strings.  However, please note that C strings should usually be marshalled
using the functions provided by "Foreign.C.String" as
the Unicode encoding has to be taken into account.  All functions specifically
operating on arrays that are terminated by a special termination element have
a name ending on @0@---e.g., 'mallocArray' allocates space for an
array of the given size, whereas 'mallocArray0' allocates space for one
more element to ensure that there is room for the terminator.
-}
module Foreign.Marshal.Array (
  -- * Marshalling arrays

  -- ** Allocation
  --
  mallocArray,    -- :: Storable a => Int -> IO (Ptr a)
  mallocArray0,   -- :: Storable a => Int -> IO (Ptr a)

  allocaArray,    -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b
  allocaArray0,   -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b

  reallocArray,   -- :: Storable a => Ptr a -> Int -> IO (Ptr a)
  reallocArray0,  -- :: Storable a => Ptr a -> Int -> IO (Ptr a)

  -- ** Marshalling
  --
  peekArray,      -- :: Storable a =>         Int -> Ptr a -> IO [a]
  peekArray0,     -- :: (Storable a, Eq a) => a   -> Ptr a -> IO [a]

  pokeArray,      -- :: Storable a =>      Ptr a -> [a] -> IO ()
  pokeArray0,     -- :: Storable a => a -> Ptr a -> [a] -> IO ()

  -- ** Combined allocation and marshalling
  --
  newArray,       -- :: Storable a =>      [a] -> IO (Ptr a)
  newArray0,      -- :: Storable a => a -> [a] -> IO (Ptr a)

  withArray,      -- :: Storable a =>      [a] -> (Ptr a -> IO b) -> IO b
  withArray0,     -- :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b

  withArrayLen,   -- :: Storable a =>      [a] -> (Int -> Ptr a -> IO b) -> IO b
  withArrayLen0,  -- :: Storable a => a -> [a] -> (Int -> Ptr a -> IO b) -> IO b

  -- ** Copying

  -- | (argument order: destination, source)
  copyArray,      -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
  moveArray,      -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()

  -- ** Finding the length
  --
  lengthArray0,   -- :: (Storable a, Eq a) => a -> Ptr a -> IO Int

  -- ** Indexing
  --
  advancePtr,     -- :: Storable a => Ptr a -> Int -> Ptr a
  ) where
import qualified "base" Foreign.Marshal.Array as Base
import "base" Foreign.Marshal.Array hiding (peekArray)
#if __GLASGOW_HASKELL__ >= 701
import "base" Foreign.Safe hiding (peekArray)
#else
import "base" Foreign hiding (peekArray)
#endif

-- |Convert an array of given length into a Haskell list.
--
peekArray          :: Storable a => Int -> Ptr a -> IO [a]
peekArray = Base.peekArray
