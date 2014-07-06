module Example where

import Data.Char

-- Problem: there are several ways to fix the program. No oracle given.                
-- DZ: added the defnition of "plus"
plus :: Int -> Int -> Int
plus = (+)

v41 = \a -> plus ((\b -> if b then b else a) True, 3)

-- 10,18-53
