module Propositie where
import Data.List

eqString      :: String -> String -> Bool 
eqString = undefined

data Prop
     = En [Prop]
     | Of [Prop]
     | Niet Prop
     | Prop :-> Prop
     | Var String
     | Bool Bool

evalueer :: Prop -> Bedeling -> Bool


type Bedeling = [String]

evalueer (En []) _ = True
evalueer (En (x:xs)) y = testVoorkomen x y && evalueer (En xs) y
evalueer (Of []) _ = True
evalueer (Of (x:xs)) y = testVoorkomen x y || evalueer (Of xs) y
evalueer (Niet _) [] = True
evalueer (Niet x)  (y:ys) = test x y && evalueer (Niet x) ys
evalueer (a:->b) y         | evalueer (Niet a) y = True
                           | otherwise = testVoorkomen b y

testVoorkomen :: Prop -> [String] -> Bool

testVoorkomen _ [] = False
testVoorkomen (Var a) (b:bs) = eqString a b || testVoorkomen (Var a) bs

test :: Prop -> String -> Bool
test (Var a) b   | eqString a b = False
                 | otherwise = True

vervulbaar :: Prop -> [Bedeling]

vervulbaar (En[]) = []
vervulbaar (En x) =  filter deelVan (powerSet(alleVariabelen (En x)))
                                   where deelVan a = evalueer (En x) a
vervulbaar (Of[]) =  []
vervulbaar (Of x) =  filter deelVan (powerSet(alleVariabelen (Of x)))
                                   where deelVan a = evalueer (Of x) a

vervulbaar (Niet x)= filter deelVan (powerSet(alleVariabelen (Niet x)))
                             where deelVan a = evalueer (Niet x) a
vervulbaar (x :-> y) =  filter deelVan (powerSet(alleVariabelen (x :-> y)))
                                   where deelVan a = evalueer (x:->y) a


powerSet :: [String] -> [Bedeling]
powerSet [] = [[]]
powerSet (x:xs)  = map (x:) (powerSet xs) ++ powerSet xs


alleVariabelen :: Prop -> [String]

alleVariabelen (En[])   = []
alleVariabelen (En x)   = concatMap alleVariabelen x
alleVariabelen (Of[])   = []
alleVariabelen (Of x)   = concatMap (nubBy eqString alleVariabelen) x
alleVariabelen (Niet x) = alleVariabelen x
alleVariabelen(p :-> q) = nubBy eqString (alleVariabelen p ++ alleVariabelen q)
alleVariabelen(Bool _)  = []
alleVariabelen(Var s)   = s : []

tautologie :: Prop -> Bool

tautologie (En x) |((length (vervulbaar (En x)))== (length (alleVariabelen( En x)))) = True
                  | otherwise = False

tautologie (Of x) = vergelijkbed (powerSet(alleVariabelen (Of x))) (vervulbaar(Of x))


vergelijkbed [] [] = True
vergelijkbed _  [] = False
vergelijkbed (x:xs) (y:ys) = vergelijkstr x y && vergelijkbed xs ys

vergelijkstr [] [] = True
vergelijkstr _  [] = False
vergelijkstr (x:xs) (y:ys) = eqString x y && vergelijkstr xs ys

-- the expression should be: alleVariabelen
-- 63,38-66
