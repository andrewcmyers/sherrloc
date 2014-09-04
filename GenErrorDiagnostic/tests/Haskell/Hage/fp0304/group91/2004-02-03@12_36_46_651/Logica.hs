module Logica where

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]



evalueer :: Prop -> Bedeling -> Prop
evalueer p b = head p

-- head p should be: p
-- 16,16-21   16,21-21
