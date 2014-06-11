{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , MagicHash
           , GeneralizedNewtypeDeriving
  #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Ptr
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides typed pointers to foreign data.  It is part
-- of the Foreign Function Interface (FFI) and will normally be
-- imported via the "Foreign" module.
--
-----------------------------------------------------------------------------

module Foreign.Ptr (

    -- * Data pointers

    Ptr,
    nullPtr,
    castPtr,
    plusPtr,
    alignPtr,
    minusPtr,

    -- * Function pointers

    FunPtr,
    nullFunPtr,
    castFunPtr,
    castFunPtrToPtr,
    castPtrToFunPtr,

    freeHaskellFunPtr,
    -- Free the function pointer created by foreign export dynamic.

    -- * Integral types with lossless conversion to and from pointers
    IntPtr,
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr,
    ptrToWordPtr,
    wordPtrToPtr
 ) where

import GHC.Ptr
import GHC.Base
import GHC.Num
import GHC.Read
import GHC.Real
import GHC.Show
import GHC.Enum

import Data.Bits
import Data.Typeable
import Foreign.Storable ( Storable(..) )

-- | Release the storage associated with the given 'FunPtr', which
-- must have been obtained from a wrapper stub.  This should be called
-- whenever the return value from a foreign import wrapper function is
-- no longer required; otherwise, the storage it uses will leak.
foreign import ccall unsafe "freeHaskellFunctionPtr"
    freeHaskellFunPtr :: FunPtr a -> IO ()

#include "HsBaseConfig.h"
#include "CTypes.h"

-- | An unsigned integral type that can be losslessly converted to and from
-- @Ptr@. This type is also compatible with the C99 type @uintptr_t@, and
-- can be marshalled to and from that type safely.
INTEGRAL_TYPE(WordPtr,Word)
        -- Word and Int are guaranteed pointer-sized in GHC

-- | A signed integral type that can be losslessly converted to and from
-- @Ptr@.  This type is also compatible with the C99 type @intptr_t@, and
-- can be marshalled to and from that type safely.
INTEGRAL_TYPE(IntPtr,Int)
        -- Word and Int are guaranteed pointer-sized in GHC

-- | casts a @Ptr@ to a @WordPtr@
ptrToWordPtr :: Ptr a -> WordPtr
ptrToWordPtr (Ptr a#) = WordPtr (W# (int2Word# (addr2Int# a#)))

-- | casts a @WordPtr@ to a @Ptr@
wordPtrToPtr :: WordPtr -> Ptr a
wordPtrToPtr (WordPtr (W# w#)) = Ptr (int2Addr# (word2Int# w#))

-- | casts a @Ptr@ to an @IntPtr@
ptrToIntPtr :: Ptr a -> IntPtr
ptrToIntPtr (Ptr a#) = IntPtr (I# (addr2Int# a#))

-- | casts an @IntPtr@ to a @Ptr@
intPtrToPtr :: IntPtr -> Ptr a
intPtrToPtr (IntPtr (I# i#)) = Ptr (int2Addr# i#)
