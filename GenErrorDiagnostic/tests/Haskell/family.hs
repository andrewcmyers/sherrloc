{-# LANGUAGE TypeFamilies #-}
module Example where

type family F a
type instance F Bool = Int

foo :: F Bool -> Int
foo n = True
-- 8,9-12
