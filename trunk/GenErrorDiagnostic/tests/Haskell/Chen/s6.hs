{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: the arguments to c must have the same type
class C a b | a -> b where
    c :: a -> b -> a
    
instance C a a

e = c (\x -> x) 'a'

-- Parsing error
