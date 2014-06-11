{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Foreign.StablePtr
        ( -- * Stable references to Haskell values
          StablePtr          -- abstract
        , newStablePtr       -- :: a -> IO (StablePtr a)
        , deRefStablePtr     -- :: StablePtr a -> IO a
        , freeStablePtr      -- :: StablePtr a -> IO ()
        , castStablePtrToPtr -- :: StablePtr a -> Ptr ()
        , castPtrToStablePtr -- :: Ptr () -> StablePtr a
        , -- ** The C-side interface

          -- $cinterface
  ) where
import "base" Foreign.StablePtr as This___

-- $cinterface
--
-- The following definition is available to C programs inter-operating with
-- Haskell code when including the header @HsFFI.h@.
--
-- > typedef void *HsStablePtr;  /* C representation of a StablePtr */
--
-- Note that no assumptions may be made about the values representing stable
-- pointers.  In fact, they need not even be valid memory addresses.  The only
-- guarantee provided is that if they are passed back to Haskell land, the
-- function 'deRefStablePtr' will be able to reconstruct the
-- Haskell value referred to by the stable pointer.
