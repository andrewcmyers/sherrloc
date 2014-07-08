{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: empty's type is always ambiguous
class Collects c e where
    insert :: e -> c -> c
    lookup :: c -> Int -> Maybe e
    empty :: c
    
init e = insert e empty

-- 13,19-23 
