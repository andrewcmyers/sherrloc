{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Data.Word (
        -- * Unsigned integral types

        -- $notes

        Word,
        Word8, Word16, Word32, Word64,

  ) where
import "base" Data.Word

-- SDM: removed after 'Prelude.fromIntegral':
-- ..., which is specialized for all the common cases
-- so should be fast enough

-- SDM: removed: It would be very natural to add a type @Natural@ providing an
-- unbounded size unsigned integer, just as 'Prelude.Integer' provides
-- unbounded size signed integers.  We do not do that yet since there is
-- no demand for it.

-- SDM: removed, after "All arithmetic is performed module 2^n...".  Neither
-- Ian Lynagh nor I understand what this means:
--   One non-obvious consequence of this is that 'Prelude.negate'
--   should /not/ raise an error on negative arguments.

{- $notes

This module provides unsigned integer types of unspecified width ('Word')
and fixed widths ('Word8', 'Word16', 'Word32' and 'Word64').  All
arithmetic is performed modulo 2^n, where @n@ is the number of bits in
the type.

For coercing between any two integer types, use
'Prelude.fromIntegral'.  Coercing word types to and from integer
types preserves representation, not sign.

The rules that hold for 'Prelude.Enum' instances over a bounded type
such as 'Prelude.Int' (see the section of the Haskell language report dealing
with arithmetic sequences) also hold for the 'Prelude.Enum' instances
over the various 'Word' types defined here.

Right and left shifts by amounts greater than or equal to the width of
the type result in a zero result.  This is contrary to the behaviour
in C, which is undefined; a common interpretation is to truncate the
shift count to the width of the type, for example @1 \<\< 32 == 1@ in
some C implementations.

-}

