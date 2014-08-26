module Prop(module List, module Prop) where
import Data.List

type Bedeling = [String]

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (En p) b = and $ map (`evalueer` b) p
evalueer (Of p) b = or $ map (`evalueer` b) p

evalueer (y :-> z) b = (not $ evalueer y b) || (evalueer z b)
evalueer (Bool y) _ = y
evalueer (Var y) b = zoekOp b (Var y)
evalueer (Niet y) b = not $ evalueer y b

zoekOp :: Bedeling -> Prop -> Bool
zoekOp b (Var v) = elemBy eqString v b
zoekOp _ _ = False

vervulbaar :: Prop -> [Bedeling]
vervulbaar p = filter (evalueer p) (deelRij $ nubBy eqString (krijgLijst p))

krijgLijst :: Prop -> [String]
krijgLijst (Var y)     = [y]
krijgLijst (En xs)     = concatMap krijgLijst xs
krijgLijst (Of xs)     = concatMap krijgLijst xs
krijgLijst (Niet x)    = krijgLijst x
krijgLijst (y :-> z)   = krijgLijst y ++ krijgLijst z
krijgLijst (Bool _)    = []

deelRij :: [a] -> [[a]]
deelRij [] = [[]]
deelRij (x:xs) = (deelRij xs) ++ map(x:) (deelRij xs)

tautologie :: Prop -> Bool
tautologie p = eqList (eqList eqString) (deelRij $ (nubBy eqString (krijgLijst p))) (vervulbaar p)

contradictie :: Prop->Bool
contradictie p = null (vervulbaar p)

equivalent :: Prop -> Prop -> Bool
equivalent p1 p2 = eqList eqString (vervulbaar p1) (vervulbaar p2)

-- eqString should be: eqList eqString
-- 56,27-34
