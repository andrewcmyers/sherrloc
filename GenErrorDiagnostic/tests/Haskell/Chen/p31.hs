module Example where

import Data.Char

-- Problem: "True" should be True
string2bool :: String -> Bool
string2bool = undefined
not x = if x then string2bool "False"           
             else fst ("True", 1)
             
-- 9,24-29 
