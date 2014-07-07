
{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}
-- tcfail121.hs

module ShouldFail where

class Foo a where
  op :: a -> a

instance Foo a => Foo [a] 
instance Foo [Int]

foo :: Foo a => [a] -> [a]
foo x = op x
-- 14,9-10
