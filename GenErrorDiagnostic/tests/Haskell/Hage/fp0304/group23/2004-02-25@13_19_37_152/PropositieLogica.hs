module PropositieLogica where

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

data OString
  = String :/\\ String
  | String :\\/ String

type Bedeling = [String]

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> [String] -> Bool
evalueer (Bool b) _     = b
evalueer (Var a) []     = False
evalueer (Var a) x |elemBy eqString a x = True
                   |otherwise = False
evalueer (En []) _         = True
evalueer (En (x:xs)) y     = evalueer x y  && evalueer (En xs) y
evalueer (Of []) _         = False
evalueer (Of (x:xs)) y     = evalueer x y  || evalueer (Of xs) y
evalueer (Niet x) y        = not (evalueer x y)
evalueer (a:->b) y | ((eqBool(evalueer a y) True)&&(eqBool(evalueer b y) False )) = False
                    |otherwise = True

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
            where subsxs = subs xs


removedb :: [String] -> [String]
removedb [] = []
removedb (x:xs)|elemBy eqString x xs = removedb xs
               |otherwise = x:(removedb xs)


toparts :: Prop -> [String]
toparts (Var a)     = [a]
toparts (Bool b)    | True =[]
                    | False =[]
toparts (En [])     = []
toparts (Of [])     = []
toparts (En (x:xs)) = (removedb((toparts x) ++ (toparts (En xs))))
toparts (Of (x:xs)) = (removedb((toparts x) ++ (toparts (Of xs))))
toparts (Niet x)    = removedb(toparts x)
toparts (a:->b)     = removedb((toparts a) ++ (toparts b))


takert :: [Bool] -> [[String]] -> [[String]]
takert [] [] = []
takert (x:xs) (y:ys) | x        = y : (takert xs ys)
                     |otherwise = takert xs ys


vervulbaar :: Prop -> [Bedeling]
vervulbaar  x = takert (map(evalueer x) (subs(toparts x)))  (subs(toparts x))

isEq :: [[String]] -> [[String]] -> Bool
isEq [[]] [[]] = False
isEq x [] = True
isEq [] x = True
isEq (x:xs) (y:ys) = not( eqList eqString x  y) && isEq xs ys


tautologie :: Prop -> Bool
tautologie x |eqList (eqList eqString) (vervulbaar x) (subs(toparts x))= True
             |otherwise = False






contradictie :: Prop -> Bool
contradictie x | not(isEq (vervulbaar x) (subs(toparts x)))=False
                |otherwise =True



toon  :: Prop -> String
toon (Var a)      = a
toon (Niet a)     ="!"++toon a
toon (Bool b)       | eqBool b True ="True"
                    | eqBool b False="False"
toon (En [last])  = toon last
toon (En (x:xs))  = "("++toon x++" /\\ " ++toon(En xs)++")"
toon (Of [last])  = toon last
toon (Of (x:xs))  = "("++toon x++" \\/ " ++toon(Of xs)++")"
toon (a:->b)   = "("++toon a++" -> "  ++toon b++")"


equivalent :: Prop -> Prop -> Bool
equivalent propa propb =eqList (eqList eqString) (vervulbaar (checkProp(propa))) (vervulbaar (checkProp(propb)))


checkProp :: Prop->Prop
checkProp p| eqBool (tautologie p) True = Bool True
           | eqBool (contradictie p) True =Bool False
           | otherwise =p


deMorgan :: Prop -> Prop
deMorgan (Var a)  = Var a
deMorgan (Bool b) = Bool b
deMorgan (a:->b)  = deMorgan a:-> deMorgan b
deMorgan (En (x:xs))    = En(map deMorgan (x:xs))
deMorgan (Of (x:xs))    = Of(map deMorgan (x:xs))
deMorgan (Niet (En x)) = deMorgan (Of( map reWrite x))
deMorgan (Niet (Of x)) = deMorgan (En( map reWrite x))
deMorgan (Niet (Var x))= Niet (Var x)
deMorgan (Niet (Bool b))= Niet (Bool b)
deMorgan (Niet (a:->b)) = Niet (a:->b)


reWrite :: Prop -> Prop
reWrite x          =Niet x



tempOntleed :: OString -> Prop
tempOntleed (a :/\\ b)  = Of(Var a,Var b)

-- (Var a, Var b) should be: [Var a, Var b]
-- 137,29-41   
