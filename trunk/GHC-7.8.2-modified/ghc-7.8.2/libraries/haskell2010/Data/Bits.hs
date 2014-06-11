{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-- |
-- This module defines bitwise operations for signed and unsigned
-- integers.

module Data.Bits (
  Bits(
    (.&.), (.|.), xor, -- :: a -> a -> a
    complement,        -- :: a -> a
    shift,             -- :: a -> Int -> a
    rotate,            -- :: a -> Int -> a
    bit,               -- :: Int -> a
    setBit,            -- :: a -> Int -> a
    clearBit,          -- :: a -> Int -> a
    complementBit,     -- :: a -> Int -> a
    testBit,           -- :: a -> Int -> Bool
    bitSize,           -- :: a -> Int
    isSigned,          -- :: a -> Bool
    shiftL, shiftR,    -- :: a -> Int -> a
    rotateL, rotateR   -- :: a -> Int -> a
  )
  ) where
import "base" Data.Bits
