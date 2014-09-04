module Practicum1af3 where
import Data.List

type Table = [[String]]

eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined


join_fix1 :: Table -> Table -> Table -> Table
join_fix1 ((x:xs):xss) ((y:ys):yss) z | eqString x y = (x:xs)
                                      | True         = join_fix1 (xs:xss) (ys:yss) z
join_fix1 ([]:xss) _ z = join_fix2 xss z
join_fix1 _ _ _ = []

join_fix2 :: Table -> Table -> Table
join_fix2 ((x:xs):xss) ((y:ys):yss) | eqString x y = (x:xs)
                                    | True         = join_fix2 xss ((y:ys):yss)
join_fix2 _ _ = []

-- missing []
-- 112,57-60   118,55-58
