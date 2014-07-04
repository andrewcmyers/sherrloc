module Example where

import Data.Char

-- Problem: False should be "False"
showInt x =                                         
    case x of
        0 -> False
        1 -> "one"
        2 -> "two"
        _ -> "many"

-- 8,14-18
