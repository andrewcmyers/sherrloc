{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Example where

import Data.Char

-- Problem: inferred type doesn't subsume declared type
notEq :: Eq a => a -> b -> Bool
notEq x y = if x == y then False else True


----------------------------------
-- Top quality type error messages

-- 8,10-31
