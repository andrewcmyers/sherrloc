module T where

splits :: [a] -> ([a],[a])
splits xs = map (\n -> splitAt n xs) [0 length xs]

-- [0 length xs] should be [0 ... length xs]
-- 4,39-49
