{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Data.List (
   -- * Basic functions

     (++)              -- :: [a] -> [a] -> [a]
   , head              -- :: [a] -> a
   , last              -- :: [a] -> a
   , tail              -- :: [a] -> [a]
   , init              -- :: [a] -> [a]
   , null              -- :: [a] -> Bool
   , length            -- :: [a] -> Int

   -- * List transformations
   , map               -- :: (a -> b) -> [a] -> [b]
   , reverse           -- :: [a] -> [a]

   , intersperse       -- :: a -> [a] -> [a]
   , intercalate       -- :: [a] -> [[a]] -> [a]
   , transpose         -- :: [[a]] -> [[a]]

   , subsequences      -- :: [a] -> [[a]]
   , permutations      -- :: [a] -> [[a]]

   -- * Reducing lists (folds)

   , foldl             -- :: (a -> b -> a) -> a -> [b] -> a
   , foldl'            -- :: (a -> b -> a) -> a -> [b] -> a
   , foldl1            -- :: (a -> a -> a) -> [a] -> a
   , foldl1'           -- :: (a -> a -> a) -> [a] -> a
   , foldr             -- :: (a -> b -> b) -> b -> [a] -> b
   , foldr1            -- :: (a -> a -> a) -> [a] -> a

   -- ** Special folds

   , concat            -- :: [[a]] -> [a]
   , concatMap         -- :: (a -> [b]) -> [a] -> [b]
   , and               -- :: [Bool] -> Bool
   , or                -- :: [Bool] -> Bool
   , any               -- :: (a -> Bool) -> [a] -> Bool
   , all               -- :: (a -> Bool) -> [a] -> Bool
   , sum               -- :: (Num a) => [a] -> a
   , product           -- :: (Num a) => [a] -> a
   , maximum           -- :: (Ord a) => [a] -> a
   , minimum           -- :: (Ord a) => [a] -> a

   -- * Building lists

   -- ** Scans
   , scanl             -- :: (a -> b -> a) -> a -> [b] -> [a]
   , scanl1            -- :: (a -> a -> a) -> [a] -> [a]
   , scanr             -- :: (a -> b -> b) -> b -> [a] -> [b]
   , scanr1            -- :: (a -> a -> a) -> [a] -> [a]

   -- ** Accumulating maps
   , mapAccumL         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
   , mapAccumR         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])

   -- ** Infinite lists
   , iterate           -- :: (a -> a) -> a -> [a]
   , repeat            -- :: a -> [a]
   , replicate         -- :: Int -> a -> [a]
   , cycle             -- :: [a] -> [a]

   -- ** Unfolding
   , unfoldr           -- :: (b -> Maybe (a, b)) -> b -> [a]

   -- * Sublists

   -- ** Extracting sublists
   , take              -- :: Int -> [a] -> [a]
   , drop              -- :: Int -> [a] -> [a]
   , splitAt           -- :: Int -> [a] -> ([a], [a])

   , takeWhile         -- :: (a -> Bool) -> [a] -> [a]
   , dropWhile         -- :: (a -> Bool) -> [a] -> [a]
   , span              -- :: (a -> Bool) -> [a] -> ([a], [a])
   , break             -- :: (a -> Bool) -> [a] -> ([a], [a])

   , stripPrefix       -- :: Eq a => [a] -> [a] -> Maybe [a]

   , group             -- :: Eq a => [a] -> [[a]]

   , inits             -- :: [a] -> [[a]]
   , tails             -- :: [a] -> [[a]]

   -- ** Predicates
   , isPrefixOf        -- :: (Eq a) => [a] -> [a] -> Bool
   , isSuffixOf        -- :: (Eq a) => [a] -> [a] -> Bool
   , isInfixOf         -- :: (Eq a) => [a] -> [a] -> Bool

   -- * Searching lists

   -- ** Searching by equality
   , elem              -- :: a -> [a] -> Bool
   , notElem           -- :: a -> [a] -> Bool
   , lookup            -- :: (Eq a) => a -> [(a,b)] -> Maybe b

   -- ** Searching with a predicate
   , find              -- :: (a -> Bool) -> [a] -> Maybe a
   , filter            -- :: (a -> Bool) -> [a] -> [a]
   , partition         -- :: (a -> Bool) -> [a] -> ([a], [a])

   -- * Indexing lists
   -- | These functions treat a list @xs@ as a indexed collection,
   -- with indices ranging from 0 to @'length' xs - 1@.

   , (!!)              -- :: [a] -> Int -> a

   , elemIndex         -- :: (Eq a) => a -> [a] -> Maybe Int
   , elemIndices       -- :: (Eq a) => a -> [a] -> [Int]

   , findIndex         -- :: (a -> Bool) -> [a] -> Maybe Int
   , findIndices       -- :: (a -> Bool) -> [a] -> [Int]

   -- * Zipping and unzipping lists

   , zip               -- :: [a] -> [b] -> [(a,b)]
   , zip3
   , zip4, zip5, zip6, zip7

   , zipWith           -- :: (a -> b -> c) -> [a] -> [b] -> [c]
   , zipWith3
   , zipWith4, zipWith5, zipWith6, zipWith7

   , unzip             -- :: [(a,b)] -> ([a],[b])
   , unzip3
   , unzip4, unzip5, unzip6, unzip7

   -- * Special lists

   -- ** Functions on strings
   , lines             -- :: String   -> [String]
   , words             -- :: String   -> [String]
   , unlines           -- :: [String] -> String
   , unwords           -- :: [String] -> String

   -- ** \"Set\" operations

   , nub               -- :: (Eq a) => [a] -> [a]

   , delete            -- :: (Eq a) => a -> [a] -> [a]
   , (\\)              -- :: (Eq a) => [a] -> [a] -> [a]

   , union             -- :: (Eq a) => [a] -> [a] -> [a]
   , intersect         -- :: (Eq a) => [a] -> [a] -> [a]

   -- ** Ordered lists
   , sort              -- :: (Ord a) => [a] -> [a]
   , insert            -- :: (Ord a) => a -> [a] -> [a]

   -- * Generalized functions

   -- ** The \"@By@\" operations
   -- | By convention, overloaded functions have a non-overloaded
   -- counterpart whose name is suffixed with \`@By@\'.

   -- *** User-supplied equality (replacing an @Eq@ context)
   -- | The predicate is assumed to define an equivalence.
   , nubBy             -- :: (a -> a -> Bool) -> [a] -> [a]
   , deleteBy          -- :: (a -> a -> Bool) -> a -> [a] -> [a]
   , deleteFirstsBy    -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
   , unionBy           -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
   , intersectBy       -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
   , groupBy           -- :: (a -> a -> Bool) -> [a] -> [[a]]

   -- *** User-supplied comparison (replacing an @Ord@ context)
   -- | The function is assumed to define a total ordering.
   , sortBy            -- :: (a -> a -> Ordering) -> [a] -> [a]
   , insertBy          -- :: (a -> a -> Ordering) -> a -> [a] -> [a]
   , maximumBy         -- :: (a -> a -> Ordering) -> [a] -> a
   , minimumBy         -- :: (a -> a -> Ordering) -> [a] -> a

   -- ** The \"@generic@\" operations
   -- | The prefix \`@generic@\' indicates an overloaded function that
   -- is a generalized version of a "Prelude" function.

   , genericLength     -- :: (Integral a) => [b] -> a
   , genericTake       -- :: (Integral a) => a -> [b] -> [b]
   , genericDrop       -- :: (Integral a) => a -> [b] -> [b]
   , genericSplitAt    -- :: (Integral a) => a -> [b] -> ([b], [b])
   , genericIndex      -- :: (Integral a) => [b] -> a -> b
   , genericReplicate  -- :: (Integral a) => a -> b -> [b]

  ) where
import "base" Data.List hiding ( splitAt )
