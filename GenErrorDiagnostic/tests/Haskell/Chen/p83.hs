module Example where

import Data.Char

-- Problem: ["error"] and "location" should be swapped
add1 str lst                                                         
    | str `elem` lst = lst
    | True = str:lst
v51 = add1 ["error"] "location"

-- 9,12-31    9,7-31
