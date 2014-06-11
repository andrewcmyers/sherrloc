{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Data.Array (
    -- * Immutable non-strict arrays
    -- $intro
      module Data.Ix            -- export all of Ix
    , Array                     -- Array type is abstract

    -- * Array construction
    , array         -- :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
    , listArray     -- :: (Ix a) => (a,a) -> [b] -> Array a b
    , accumArray    -- :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
    -- * Accessing arrays
    , (!)           -- :: (Ix a) => Array a b -> a -> b
    , bounds        -- :: (Ix a) => Array a b -> (a,a)
    , indices       -- :: (Ix a) => Array a b -> [a]
    , elems         -- :: (Ix a) => Array a b -> [b]
    , assocs        -- :: (Ix a) => Array a b -> [(a,b)]
    -- * Incremental array updates
    , (//)          -- :: (Ix a) => Array a b -> [(a,b)] -> Array a b
    , accum         -- :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
    -- * Derived arrays
    , ixmap         -- :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a b

    -- * Specification

    -- $code
  ) where

import qualified "array" Data.Array as Array
import "array" Data.Array hiding (array, (//))
import "base" Data.Ix

{- $intro
Haskell provides indexable /arrays/, which may be thought of as functions
whose domains are isomorphic to contiguous subsets of the integers.
Functions restricted in this way can be implemented efficiently;
in particular, a programmer may reasonably expect rapid access to
the components.  To ensure the possibility of such an implementation,
arrays are treated as data, not as general functions.

Since most array functions involve the class 'Ix', the contents of the
module "Data.Ix" are re-exported from "Data.Array" for convenience:
-}

-- SDM: copied documentation for 'array' to remove GHC reference

-- | Construct an array with the specified bounds and containing values
-- for given indices within these bounds.
--
-- The array is undefined (i.e. bottom) if any index in the list is
-- out of bounds.  If any
-- two associations in the list have the same index, the value at that
-- index is undefined (i.e. bottom).
--
-- Because the indices must be checked for these errors, 'array' is
-- strict in the bounds argument and in the indices of the association
-- list, but non-strict in the values.  Thus, recurrences such as the
-- following are possible:
--
-- > a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])
--
-- Not every index within the bounds of the array need appear in the
-- association list, but the values associated with indices that do not
-- appear will be undefined (i.e. bottom).
--
-- If, in any dimension, the lower bound is greater than the upper bound,
-- then the array is legal, but empty.  Indexing an empty array always
-- gives an array-bounds error, but 'bounds' still yields the bounds
-- with which the array was constructed.
array :: Ix i
        => (i,i)        -- ^ a pair of /bounds/, each of the index type
                        -- of the array.  These bounds are the lowest and
                        -- highest indices in the array, in that order.
                        -- For example, a one-origin vector of length
                        -- '10' has bounds '(1,10)', and a one-origin '10'
                        -- by '10' matrix has bounds '((1,1),(10,10))'.
        -> [(i, e)]     -- ^ a list of /associations/ of the form
                        -- (/index/, /value/).  Typically, this list will
                        -- be expressed as a comprehension.  An
                        -- association '(i, x)' defines the value of
                        -- the array at index 'i' to be 'x'.
        -> Array i e
array = Array.array

-- SDM copied docs for (//) to remove GHC reference

-- | Constructs an array identical to the first argument except that it has
-- been updated by the associations in the right argument.
-- For example, if @m@ is a 1-origin, @n@ by @n@ matrix, then
--
-- > m//[((i,i), 0) | i <- [1..n]]
--
-- is the same matrix, except with the diagonal zeroed.
--
-- Repeated indices in the association list are handled as for 'array':
-- the resulting array is undefined (i.e. bottom),
(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
(//) = (Array.//)

{- $code
> module  Array (
>     module Data.Ix,  -- export all of Data.Ix
>     Array, array, listArray, (!), bounds, indices, elems, assocs,
>     accumArray, (//), accum, ixmap ) where
>
> import Data.Ix
> import Data.List( (\\) )
>
> infixl 9  !, //
>
> data (Ix a) => Array a b = MkArray (a,a) (a -> b) deriving ()
>
> array       :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
> array b ivs
>   | any (not . inRange b. fst) ivs
>      = error "Data.Array.array: out-of-range array association"
>   | otherwise
>      = MkArray b arr
>   where
>     arr j = case [ v | (i,v) <- ivs, i == j ] of
>               [v]   -> v
>               []    -> error "Data.Array.!: undefined array element"
>               _     -> error "Data.Array.!: multiply defined array element"
>
> listArray             :: (Ix a) => (a,a) -> [b] -> Array a b
> listArray b vs        =  array b (zipWith (\ a b -> (a,b)) (range b) vs)
>
> (!)                   :: (Ix a) => Array a b -> a -> b
> (!) (MkArray _ f)     =  f
>
> bounds                :: (Ix a) => Array a b -> (a,a)
> bounds (MkArray b _)  =  b
>
> indices               :: (Ix a) => Array a b -> [a]
> indices               =  range . bounds
>
> elems                 :: (Ix a) => Array a b -> [b]
> elems a               =  [a!i | i <- indices a]
>
> assocs                :: (Ix a) => Array a b -> [(a,b)]
> assocs a              =  [(i, a!i) | i <- indices a]
>
> (//)                  :: (Ix a) => Array a b -> [(a,b)] -> Array a b
> a // new_ivs          = array (bounds a) (old_ivs ++ new_ivs)
>                       where
>                         old_ivs = [(i,a!i) | i <- indices a,
>                                              i `notElem` new_is]
>                         new_is  = [i | (i,_) <- new_ivs]
>
> accum                 :: (Ix a) => (b -> c -> b) -> Array a b -> [(a,c)]
>                                    -> Array a b
> accum f               =  foldl (\a (i,v) -> a // [(i,f (a!i) v)])
>
> accumArray            :: (Ix a) => (b -> c -> b) -> b -> (a,a) -> [(a,c)]
>                                    -> Array a b
> accumArray f z b      =  accum f (array b [(i,z) | i <- range b])
>
> ixmap                 :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c
>                                          -> Array a c
> ixmap b f a           = array b [(i, a ! f i) | i <- range b]
>
> instance  (Ix a)          => Functor (Array a) where
>     fmap fn (MkArray b f) =  MkArray b (fn . f)
>
> instance  (Ix a, Eq b)  => Eq (Array a b)  where
>     a == a' =  assocs a == assocs a'
>
> instance  (Ix a, Ord b) => Ord (Array a b)  where
>     a <= a' =  assocs a <= assocs a'
>
> instance  (Ix a, Show a, Show b) => Show (Array a b)  where
>     showsPrec p a = showParen (p > arrPrec) (
>                     showString "array " .
>                     showsPrec (arrPrec+1) (bounds a) . showChar ' ' .
>                     showsPrec (arrPrec+1) (assocs a)                  )
>
> instance  (Ix a, Read a, Read b) => Read (Array a b)  where
>     readsPrec p = readParen (p > arrPrec)
>            (\r -> [ (array b as, u)
>                   | ("array",s) <- lex r,
>                     (b,t)       <- readsPrec (arrPrec+1) s,
>                     (as,u)      <- readsPrec (arrPrec+1) t ])
>
> -- Precedence of the 'array' function is that of application itself
> arrPrec = 10
-}
