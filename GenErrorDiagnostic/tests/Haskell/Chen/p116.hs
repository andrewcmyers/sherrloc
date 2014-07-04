module Example where

import Data.Char

-- Problem: the condition b should of type Bool
v74 = \a -> ( \b -> if b then b else a) True + 3              

-- 6,24-24
