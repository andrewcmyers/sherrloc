module Practicum2 where

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

evalueer :: Prop -> Bedeling -> Bool
evalueer (En p)   b  = and (map (`evalueer` b) p)
evalueer (Of p) b    = or (map (`evalueer` b) p)
evalueer (Niet p) b  = not ((`evalueer` b) p)
evalueer (p1 :-> p2) b  | (eqBool (evalueer p1 b) True)  = evalueer p2 b
                        | otherwise = True
evalueer (Bool bool) b = bool
evalueer (Var string) b = elemBy eqString string b


















vervulbaar :: Prop -> [Bedeling]
vervulbaar (Var "_") = ["_"]












joepie :: IO()
joepie = putStr "Prutswerk!!"

-- ["_"] should be [["_"]]
-- 47,24-28   47,25-27 
