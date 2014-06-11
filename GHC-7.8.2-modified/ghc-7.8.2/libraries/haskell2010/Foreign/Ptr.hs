{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-- | The module "Foreign.Ptr" provides typed pointers to foreign
-- entities.  We distinguish two kinds of pointers: pointers to data
-- and pointers to functions.  It is understood that these two kinds
-- of pointers may be represented differently as they may be
-- references to data and text segments, respectively.

module Foreign.Ptr (

    -- * Data pointers

    Ptr,      -- data Ptr a
    nullPtr,      -- :: Ptr a
    castPtr,      -- :: Ptr a -> Ptr b
    plusPtr,      -- :: Ptr a -> Int -> Ptr b
    alignPtr,     -- :: Ptr a -> Int -> Ptr a
    minusPtr,     -- :: Ptr a -> Ptr b -> Int

    -- * Function pointers

    FunPtr,      -- data FunPtr a
    nullFunPtr,      -- :: FunPtr a
    castFunPtr,      -- :: FunPtr a -> FunPtr b
    castFunPtrToPtr, -- :: FunPtr a -> Ptr b
    castPtrToFunPtr, -- :: Ptr a -> FunPtr b

    freeHaskellFunPtr, -- :: FunPtr a -> IO ()
    -- Free the function pointer created by foreign export dynamic.

    -- * Integral types with lossless conversion to and from pointers
    IntPtr,
    ptrToIntPtr,
    intPtrToPtr,
    WordPtr,
    ptrToWordPtr,
    wordPtrToPtr

  ) where
import "base" Foreign.Ptr
