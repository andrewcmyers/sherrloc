{-# LANGUAGE RankNTypes #-}
module Implication where

g x =
  let f :: forall a . a -> Int
      f y = length [x, y]
--  in g "he" -- type checking error
  in (f 'c', f True) -- should report an error
