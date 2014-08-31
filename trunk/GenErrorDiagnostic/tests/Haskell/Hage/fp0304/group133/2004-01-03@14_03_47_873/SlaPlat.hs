module SlaPlat where

slaPlat :: [[a]] -> [a]
slaPlat [] = [1]
slaPlat (a:b) = a ++ slaPlat b

-- should be []
-- 4,15-15
