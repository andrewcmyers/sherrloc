module PL where

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]
eqString      :: String -> String -> Bool 
eqString = undefined

evalueer :: Prop -> Bedeling -> Bool

evalueer (Niet prop) bedeling   = not (evalueer prop bedeling)
evalueer (Bool waarde) _        = waarde
evalueer _ []                   = False

evalueer (Var string) (x:xs)  |eqString string x = True
                              |otherwise         = evalueer (Var string) xs
evalueer (p :-> q) bedeling                      = evalueer (Of [Niet p, q]) bedeling

evalueer (Of (p:ps)) bedeling                    = or (evalueer p bedeling : evalueer (Of ps) bedeling)
evalueer (Of []) _ = False

evalueer (En (p:ps)) bedeling                    = and (evalueer p bedeling : evalueer (En ps) bedeling)
evalueer (En []) _ = True

-- should be: ... : []
-- 25,78-102   28,79-103
