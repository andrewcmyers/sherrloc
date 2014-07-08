{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: the use of > causes Eq a to be insufficient
notNull :: Eq a => [a] -> Bool
notNull xs = xs > []

-------------------------
-- Type inference and type error diagnosis

-- 9,17-17 
