module Test2 where

voegIn v li = takeWhile (< v) li ++ v ++ dropWhile (< v) li

-- v should be [v]
-- 3,37-37
