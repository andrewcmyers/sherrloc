module Sebas where

drieKopie x = [x,x,x]

slaPlat   :: [[Int]] -> [Int]
slaPlat   []   = []
slaPlat   [[]] = []

slaPlat lijst = head lijst ++ slaPlat tail lijst

-- missing () around: slaPlat tail lijst
-- 9,31-37    9,31-48
