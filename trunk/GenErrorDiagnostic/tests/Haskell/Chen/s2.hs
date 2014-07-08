{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: Functional dependencies causes error, 'a' should have type Bool
class G a b | b -> a where
    g :: a -> b
    
instance G Bool Bool

f2 = not (g (g 'a'))

-- 13,16-18 
