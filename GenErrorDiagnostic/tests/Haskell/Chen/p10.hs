module Example where

import Data.Char

-- Problem: x in the 2nd line should be [x]
reverse1 [] = []                                       
reverse1 (x:xs) = reverse1 xs ++ x
last1 xs = head (reverse1 xs)
init1 = reverse1 . tail . reverse1
rotateR xs = last1 xs : init1 xs

-- 7,34-34
