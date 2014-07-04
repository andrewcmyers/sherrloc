module Example where

import Data.Char

-- Problem: [] should be 0
sumLengths [] = []                                                  
sumLengths (xs:xss) = length xs + sumLengths xss

-- 6,17-18
