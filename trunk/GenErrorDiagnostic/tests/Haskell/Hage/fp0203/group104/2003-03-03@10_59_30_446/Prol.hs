module Prol(module List, module Prol) where

import Data.List

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling= [String]

evalueer :: Prop->Bedeling->Bool

evalueer (Bool b) _  = b

evalueer (En x:xs) bd= (evalueer x bd) && evalueer (En xs) bd
evalueer (En []) _   = False
evalueer (Of x:xs) bd= (evalueer x bd)  || evalueer (Of xs) bd
evalueer (Of []) _   = True
evalueer (Niet x)  bd= not(evalueer x bd)
evalueer (a:->b)   bd= (not(evalueer a bd)) || ((evalueer  b) bd )

-- x:xs should be (x:xs)
-- 19,14-17   21,14-17
