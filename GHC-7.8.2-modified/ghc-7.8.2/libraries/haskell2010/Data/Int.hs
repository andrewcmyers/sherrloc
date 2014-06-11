{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Data.Int (
        -- * Signed integer types

        -- $notes

        Int,
        Int8, Int16, Int32, Int64,

  ) where
import "base" Data.Int

-- SDM: removed after 'Prelude.fromIntegral':
-- ..., which is specialized for all the common cases
-- so should be fast enough

{- $notes

This module provides signed integer types of unspecified width ('Int')
and fixed widths ('Int8', 'Int16', 'Int32' and 'Int64').  All
arithmetic is performed modulo 2^n, where @n@ is the number of bits in
the type.

For coercing between any two integer types, use
'Prelude.fromIntegral'.  Coercing word types (see "Data.Word") to and
from integer types preserves representation, not sign.

The rules that hold for 'Prelude.Enum' instances over a bounded type
such as 'Int' (see the section of the Haskell language report dealing with
arithmetic sequences) also hold for the 'Prelude.Enum' instances over
the various 'Int' types defined here.

Right and left shifts by amounts greater than or equal to the width of
the type result in either zero or -1, depending on the sign of the
value being shifted.  This is contrary to the behaviour in C, which is
undefined; a common interpretation is to truncate the shift count to
the width of the type, for example @1 \<\< 32 == 1@ in some C
implementations.
-}

