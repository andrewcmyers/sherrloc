{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: only Int is defined as the Similar instance.
-- For more information, refer to page 201
class Similar a where
    (<=>) :: a -> a -> Bool
    
instance Similar Int where
    (<=>) = (==)

f x xs = [x] == xs
-- Type safe in Haskell    
