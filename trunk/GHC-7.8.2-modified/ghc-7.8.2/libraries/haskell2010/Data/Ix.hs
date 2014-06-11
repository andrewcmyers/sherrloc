{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Data.Ix (
    -- * The 'Ix' class
        Ix
          ( range       -- :: (Ix a) => (a,a) -> [a]
          , index       -- :: (Ix a) => (a,a) -> a   -> Int
          , inRange     -- :: (Ix a) => (a,a) -> a   -> Bool
          , rangeSize   -- :: (Ix a) => (a,a) -> Int
          )

    -- * Deriving Instances of @Ix@

    -- $derived
  ) where
import "base" Data.Ix

{- $derived
It is possible to derive an instance of @Ix@ automatically, using
a @deriving@ clause on a @data@ declaration.
Such derived instance declarations for the class @Ix@ are only possible
for enumerations (i.e. datatypes having
only nullary constructors) and single-constructor datatypes,
whose constituent types are instances of @Ix@.   A Haskell implementation
must provide @Ix@ instances for tuples up to at least size 15.

For an /enumeration/, the nullary constructors are assumed to be
numbered left-to-right with the indices being 0 to n-1 inclusive.
This is the same numbering defined by the @Enum@ class.  For example,
given the datatype:

> data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet

we would have:

> range   (Yellow,Blue)        ==  [Yellow,Green,Blue]
> index   (Yellow,Blue) Green  ==  1
> inRange (Yellow,Blue) Red    ==  False

For /single-constructor datatypes/, the derived instance declarations
are as shown for tuples:

> instance  (Ix a, Ix b)  => Ix (a,b) where
>         range ((l,l'),(u,u'))
>                 = [(i,i') | i <- range (l,u), i' <- range (l',u')]
>         index ((l,l'),(u,u')) (i,i')
>                 =  index (l,u) i * rangeSize (l',u') + index (l',u') i'
>         inRange ((l,l'),(u,u')) (i,i')
>                 = inRange (l,u) i && inRange (l',u') i'
>
> -- Instances for other tuples are obtained from this scheme:
> --
> --  instance  (Ix a1, Ix a2, ... , Ix ak) => Ix (a1,a2,...,ak)  where
> --      range ((l1,l2,...,lk),(u1,u2,...,uk)) =
> --          [(i1,i2,...,ik) | i1 <- range (l1,u1),
> --                            i2 <- range (l2,u2),
> --                            ...
> --                            ik <- range (lk,uk)]
> --
> --      index ((l1,l2,...,lk),(u1,u2,...,uk)) (i1,i2,...,ik) =
> --        index (lk,uk) ik + rangeSize (lk,uk) * (
> --         index (lk-1,uk-1) ik-1 + rangeSize (lk-1,uk-1) * (
> --          ...
> --           index (l1,u1)))
> --
> --      inRange ((l1,l2,...lk),(u1,u2,...,uk)) (i1,i2,...,ik) =
> --          inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
> --              ... && inRange (lk,uk) ik
-}
