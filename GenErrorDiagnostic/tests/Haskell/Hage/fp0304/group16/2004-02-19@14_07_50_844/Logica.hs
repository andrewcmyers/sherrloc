module Logica where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

type Bedeling = [String]

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool





evalueer          :: Prop      ->     Bedeling          ->      Bool
vervulbaar        :: Prop      ->     [Bedeling]
zoekVar           :: Prop      ->     Bedeling
subs              :: [a]       ->     [[a]]
tautologie :: Prop -> Bool



evalueer (En p) lst = and (map ((flip evalueer) lst) p)
evalueer (Of p) lst = or  (map ((flip evalueer) lst) p)
evalueer (Niet p) lst = not (evalueer p lst)
evalueer (p1 :-> p2) lst  =  or [not (evalueer p1 lst) , (evalueer p2 lst)]
evalueer (Var x) lst = elemBy eqString x lst
evalueer (Bool x) _ = x


vervulbaar p = filter (evalueer p) (subs (nubBy eqString (zoekVar p)))


zoekVar (En p) = concat (map zoekVar p)
zoekVar (Of p) = concat (map zoekVar p)
zoekVar (Niet p) = zoekVar p
zoekVar  (p1 :-> p2) = zoekVar p1 ++ zoekVar p2
zoekVar  (Var x) = [x]
zoekVar (Bool _) = []


subs [] = [[]]
subs (x:xs) = map (x :) subsxs ++ subsxs
     where subsxs = subs xs



tautologie p = vervulbaar (p :-> Bool True)

-- should be: length (...) > 0
-- 57,16-43
