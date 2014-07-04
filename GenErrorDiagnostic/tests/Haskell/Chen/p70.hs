module Example where

import Data.Char

-- Problem: pop requires a pair while push returns a list
idStack stk = pop (push undefined stk)                              
push top stk = (top:stk)
pop (top,stk) = stk
empty = []

-- 6,15-38  7,16-24
