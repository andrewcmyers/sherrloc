module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.            
f 'a' b z = error "’a’"                         
f c True z = error "True"
f x y z = if z then x else y
f x y z = error "last"

-- 8,28-28
