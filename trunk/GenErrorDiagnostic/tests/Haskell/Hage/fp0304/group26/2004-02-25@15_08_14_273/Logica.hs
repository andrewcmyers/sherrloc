module Logica where

import Data.List

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer             :: Prop      ->     Bedeling          ->      Bool
vervulbaar           :: Prop      ->     [Bedeling]
zoekVar              :: Prop      ->     Bedeling
subs                 :: [a]       ->     [[a]]
tautologie           :: Prop      ->     Bool
contradictie         :: Prop      ->     Bool
toon                 :: Prop      ->     String
plaatsOperator       :: String    ->     [String]          ->      String
equivalent           :: Prop      ->     Prop              ->      Bool
maakWaarheidsLijst   :: Prop      ->     [Bedeling]        ->      [Bool]
deMorgan             :: Prop      ->     Prop


type Bedeling = [String]



data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool




evalueer (En p) lst            = and (map ((flip evalueer) lst) p)
evalueer (Of p) lst            = or  (map ((flip evalueer) lst) p)
evalueer (Niet p) lst          = not (evalueer p lst)
evalueer (p1 :-> p2) lst       =  or [not (evalueer p1 lst) , (evalueer p2 lst)]
evalueer (Var x) lst           = elemBy eqString x lst
evalueer (Bool x) _            = x


vervulbaar p = filter (evalueer p) (subs (nubBy eqString (zoekVar p)))


zoekVar (En p)        = concat (map zoekVar p)
zoekVar (Of p)        = concat (map zoekVar p)
zoekVar (Niet p)      = zoekVar p
zoekVar (p1 :-> p2)   = zoekVar p1 ++ zoekVar p2
zoekVar (Var x)       = [x]
zoekVar (Bool _)      = []


subs []       = [[]]
subs (x:xs)   = map (x :) subsxs ++ subsxs
     where subsxs = subs xs


tautologie p = length (vervulbaar (p :-> Bool False))  == 0

contradictie = tautologie . Niet

toon (Niet p)             = "!" ++ toon p
toon (En p)               = "(" ++ (plaatsOperator "/\\" (map toon p)) ++ ")"
toon (Of p)               = "(" ++ (plaatsOperator "\\/" (map toon p)) ++ ")"
toon (p1 :-> p2)          = "(" ++ toon p1 ++ "->" ++ toon p2 ++ ")"
toon (Var x)              = x
toon (Bool b) | b         = "True"
              | otherwise = "False"

plaatsOperator _ (p:[])   = p
plaatsOperator op (p:ps)  = p ++ op ++ plaatsOperator op ps
plaatsOperator _ []       = ""

equivalent p1 p2 = eqList eqBool (maakWaarheidsLijst p1 lijstBedelingen) (maakWaarheidsLijst p2 lijstBedelingen)
                 where lijstBedelingen = (subs (nubBy eqString (zoekVar p1))) ++ (subs (nubBy eqString (zoekVar p2)))

maakWaarheidsLijst p lijst = map (evalueer p) lijst

deMorgan (Niet (En [p1,p2]))   = Of [Niet (deMorgan p1),Niet (deMorgan p2)]
deMorgan (Niet (Of [p1,p2]))   = En [Niet (deMorgan p1),Niet (deMorgan p2)]
deMorgan (Niet p)              = Niet (deMorgan p)
deMorgan (En p)                = En (map deMorgan p)
deMorgan (Of p)                = Of (map deMorgan p)
deMorgan (p1 :-> p2)           = ((deMorgan p1) :-> (deMorgan p2))
deMorgan (Var p)               = p
deMorgan (Bool p)              = p

-- p should be (Var p) and (Bool p) respectively
-- 93,34-34   94,34-34
