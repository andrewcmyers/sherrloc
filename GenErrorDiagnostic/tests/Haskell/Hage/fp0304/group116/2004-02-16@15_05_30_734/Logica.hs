module Logica where 

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined

data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]


evalueer :: Prop -> Bedeling -> Bool
evalueer (En props) bed = and (map kees props)
                          where kees prop = evalueer prop bed
evalueer (Of props) bed = or (map kees props)
                          where kees prop = evalueer prop bed
evalueer (Niet prop) bed = not (evalueer prop bed)
evalueer (prop1 :-> prop2) bed = or [not (evalueer prop1 bed),evalueer prop2 bed]
evalueer (Var x) bed = or (map (eqString x) bed)
evalueer (Bool x) _ = x

machtsverzameling :: [a] -> [[a]]
machtsverzameling [] = [[]]
machtsverzameling (x:xs) = map (x:) mxs ++ mxs
                           where mxs = machtsverzameling xs


vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = (filter (evalueer prop) . machtsverzameling . maakUniek eqString . vindVars) prop


maakUniek :: (a -> a -> Bool) -> [a] -> [a]
maakUniek _ [] = []
maakUniek f (x:xs) | elemBy f x xs = maakUniek f xs
                   | otherwise = x : maakUniek f xs


vindVars :: Prop -> [String]
vindVars (En props) = concatMap vindVars props
vindVars (Of props) = concatMap vindVars props
vindVars (Niet prop) = vindVars prop
vindVars (prop1 :-> prop2) = (vindVars prop1) ++ (vindVars prop2)
vindVars (Var x) = [x]
vindVars _ = []


tautologie :: Prop -> Bool
tautologie prop = (length . machtsverzameling . vindVars) prop == (length . vervulbaar) prop


contradictie :: Prop -> Bool
contradictie = null . vervulbaar


toon :: Prop -> String
toon (Var x) = x
toon (Bool x) | x = "True"
              | otherwise = "False"
toon (Niet prop) = "!" ++ toonHaakjes (<) prop (Niet prop)
toon (En []) = ""
toon (En (prop:[])) = toon prop
toon (En (prop:props)) = toonHaakjes (<=) prop (En []) ++ concatMap f props
                         where f x = " /\\ " ++ toonHaakjes (<=) x (En [])
toon (Of []) = ""
toon (Of (prop:[])) = toon prop
toon (Of (prop:props)) = toonHaakjes (<=) prop (Of []) ++ concatMap f props
                         where f x = " \\/ " ++ toonHaakjes (<=) x (Of [])
toon (prop1 :-> prop2) = t (<) prop1 ++ " -> " ++ t (<=) prop2
                         where t f p = toonHaakjes f p (prop1 :-> prop2)


toonHaakjes :: (Int -> Int -> Bool) -> Prop -> Prop -> String
toonHaakjes f prop1 prop2 | prioriteit prop1 `f` prioriteit prop2 = toon prop1
                          | otherwise = "(" ++ toon prop1 ++ ")"


prioriteit :: Prop -> Int
prioriteit (Niet _) = 1
prioriteit (En _) = 2
prioriteit (Of _) = 3
prioriteit (_ :-> _) = 4
prioriteit _ = 0



equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2 = eqList (eqString) (vervulbaar prop1) (vervulbaar prop2)



deMorgan :: Prop -> Prop
deMorgan (En props) = En (map (deMorgan) props)
deMorgan (Of props) = Of (map (deMorgan) props)
deMorgan (Niet (En props)) = deMorgan (Of (map ((Niet).(deMorgan)) props))
deMorgan (Niet (Of props)) = deMorgan (En (map ((Niet).(deMorgan)) props))
deMorgan (Niet x) = Niet (deMorgan x)
deMorgan (prop1 :-> prop2) = (deMorgan prop1) :-> (deMorgan prop2)
deMorgan (Var a) = Var a
deMorgan (Bool x) = Bool x

-- eqString should be (eqList eqString)
-- 96,34-41
