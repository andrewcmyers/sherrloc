module Example where

import Data.Char

-- Problem: + should be ++
sumLengths [] = []
sumLengths (xs:xss) = xs + sumLengths xss        

-- Type safe in Haskell
