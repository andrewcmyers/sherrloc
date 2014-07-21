{-# LANGUAGE TypeFamilies #-}
module Example where
type family F a
 
f :: F a -> F a
f = undefined
  
g :: F Int -> F Int
g x = f x 
