{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: 'a' and True should have the same type
class Collects ce e | ce -> e where
    empty :: ce
    insert :: e -> ce -> ce
    
f c = insert 'a' (insert True c)

-- 12,14-16  12,26-29
