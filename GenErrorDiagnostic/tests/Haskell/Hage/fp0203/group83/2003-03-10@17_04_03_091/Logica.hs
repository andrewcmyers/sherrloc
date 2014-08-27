module Logica where
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]

eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool

evalueer (Var(x)) (element:elementen) | (eqString x element) = True
                                      | otherwise = evalueer (Var(x)) elementen
evalueer (Var(_)) [] = False

evalueer (En(a:rest)) lijst = and ((evalueer a lijst):((evalueer (En(rest)) lijst):[]))
evalueer (En([])) _ = True

evalueer (Of(a:rest)) lijst = or ((evalueer a lijst):((evalueer (Of(rest)) lijst):[]))
evalueer (Of([])) _ = False

evalueer (Niet(a)) lijst = not (evalueer a lijst)
evalueer (Bool a) _ = a

evalueer (a :-> b) lijst = (evalueer (Of((Niet(a)):(b):[])) lijst)


vervulbaar :: Prop -> [Bedeling]
vervulbaar a = test a (subs (verwijder (neemvars a)))

test :: Prop -> [Bedeling] -> [Bedeling]
test _ [] = []
test a (x:xs) | evalueer a x = x:(test a xs)
              | otherwise = test a xs

neemvars :: Prop -> [String]

neemvars (Var(x)) = x:[]

neemvars (En([])) = []
neemvars (En(a:rest)) = neemvars a ++ (neemvars (En(rest)))

neemvars (Of([])) = []
neemvars (Of(a:rest)) = neemvars a ++ (neemvars (Of(rest)))
neemvars (Niet(a)) = neemvars a
neemvars (Bool _) = []
neemvars (a :-> b) = neemvars (En(a:b:[]))


isin :: String -> [String] -> Bool
isin _ [] = False
isin x (y:ys) | eqString x y = True
              | otherwise = isin x ys

verwijder :: [String] -> [String]
verwijder [] = []
verwijder (x:xs) | isin x xs = verwijder xs
                 | otherwise = x:verwijder xs


subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) (subs xs) ++ (subs xs)



tautologie :: Prop -> Bool
tautologie a = and (map (evalueer a)(subs (verwijder (neemvars a))))



contradictie :: Prop -> Bool
contradictie a = not (tautologie a)



toon :: Prop -> String

toon (Var(x)) = x
toon (En(a:[])) = "(" ++ toon a++ ")"
toon (En(a:rest)) = "(" ++ toon a ++ ") /\\ " ++ toon (En(rest))
toon (Of(a:[])) = "(" ++ toon a++ ")"
toon (Of(a:rest)) = "(" ++ toon a ++ ") \\/ " ++ toon (Of(rest))
toon (Niet(a)) = "!"++toon a
toon (a :-> b) = "(" ++ toon a ++ ") -> " ++ toon b
toon (Bool(True)) = "True"
toon (Bool(False)) = "False"



uberlijst :: Prop -> Prop -> [[String]]
uberlijst a b = (subs (verwijder (neemvars a ++ neemvars b)))

equivalent :: Prop -> Prop -> Bool
equivalent a b = eqList eqBool (map (evalueer a) (uberlijst a b)) (map (evalueer b) (uberlijst a b))



deMorgan :: Prop -> Prop
deMorgan (Var(x)) = (Var(x))
deMorgan (En(lijst)) = (En(morgan lijst))
deMorgan (Of(lijst)) = (Of(morgan lijst))
deMorgan (Niet(En(lijst))) = Of(nieten lijst)
deMorgan (Niet(Of(lijst))) = En(nieten lijst)
deMorgan (a :-> b) = ((deMorgan a) :-> (deMorgan b))
deMorgan (Niet(x)) = (Niet(deMorgan x))
deMorgan (Bool(a)) = a

nieten :: [Prop] -> [Prop]
nieten [] = []
nieten (x:xs) = (deMorgan (Niet(x))):(nieten xs)

morgan :: [Prop] -> [Prop]
morgan [] = []
morgan (a:rest) = (deMorgan a):(morgan rest)

-- a should be Bool(a)
-- 116,22-22
