module Example where

import Data.Char

-- Problem: (nth search) misses an argument of type Int
nth = (!!)
final moves idx search = 
    if (idx == moves -1) 
    then []
    else (nth search) : final moves (idx+1) search ++ search


---------------
-- repairing type errors in functional programs    

--10,10-21 
