module PropLog where
data Prop
  = En [Prop]
  | Of [Prop]
  | Niet Prop
  | Prop :-> Prop
  | Var String
  | Bool Bool

type Bedeling = [String]
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer   :: Prop   ->  Bedeling  -> Bool
evalueer   (Bool b) _                 = b
evalueer   (Var a) bedeling           = elemBy eqString a bedeling
evalueer   (En proplijst) bedeling    = foldr (&&) True (map (flip evalueer bedeling) proplijst)
evalueer   (Of proplijst) bedeling    = foldr (||) False (map (flip evalueer bedeling) proplijst)
evalueer   (Niet prop) bedeling       = not (evalueer prop bedeling)
evalueer   (prop1 :-> prop2) bedeling = not (evalueer prop1 bedeling) || evalueer prop2 bedeling





vervulbaar   :: Prop   -> [Bedeling]
vervulbaar prop = filter (evalueer prop) (maxCombi(dubbelUit(pakVar prop)))

pakVar :: Prop -> [String]
pakVar (Bool _)          = []
pakVar (Var a)           = [a]
pakVar (En proplijst)    = concatMap pakVar proplijst
pakVar (Of proplijst)    = concatMap pakVar proplijst
pakVar (Niet proplijst)  = pakVar proplijst
pakVar (prop1 :-> prop2) = pakVar prop1 ++ pakVar prop2

dubbelUit :: [String] -> [String]
dubbelUit []     = []
dubbelUit (x:xs) = if foldr (||) False (map (eqString x) xs)
                   then dubbelUit xs
                   else x: dubbelUit xs


maxCombi :: [a] -> [[a]]
maxCombi []     = [[]]
maxCombi (x:xs) = map (x:) subsys ++ subsys
                      where subsys = maxCombi xs





tautologie   :: Prop   ->  Bool
tautologie (Bool a) = a
tautologie prop     = eqList (eqList eqString) (vervulbaar prop) (maxCombi(dubbelUit(pakVar prop)))





contradictie :: Prop   ->  Bool
contradictie (Bool a) = not a
contradictie prop     = eqList (eqList eqString) [] (vervulbaar prop)




toon         :: Prop   ->  String
toon (Bool True)       = "True"
toon (Bool False)      = "False"
toon (Var a)           = a
toon (En (x:xs))       = "(" ++ toon x ++ " /\\ " ++ toonrest (En xs)
                       where toonrest (En (y:[])) = toon y ++ ")"
                             toonrest (En (y:ys)) = toon y ++ " /\\ " ++ toonrest (En ys)
toon (Of (x:xs))       = "(" ++ toon x ++ " \\/ " ++ toonrest (Of xs)
                       where toonrest (Of (y:[])) = toon y ++ ")"
                             toonrest (Of (y:ys)) = toon y ++ " \\/ " ++ toonrest (Of ys)
toon (Niet proplijst)  = "!" ++ toon proplijst
toon (prop1 :-> prop2) = "(" ++ toon prop1 ++ " -> " ++ toon prop2 ++ ")"



equivalent prop1 prop2 = eqList (eqList eqBool) waardes1 waardes2
                                where waardes1 = map (evalueer prop1) (maxCombi (dubbelUit (pakVar prop1 ++ pakVar prop2)))
                                      waardes2 = map (evalueer prop2) (maxCombi (dubbelUit (pakVar prop1 ++ pakVar prop2)))
(/\) x y = En [x, y]
(\/) x y = Of [x, y]

deMorgan     :: Prop   ->  Prop
deMorgan (Niet(En[prop1, prop2])) = (Of [deMorgan (Niet(prop1)), deMorgan (Niet(prop2))])
deMorgan (Niet(Of[prop1, prop2])) = (En [deMorgan (Niet(prop1)), deMorgan (Niet(prop2))])
deMorgan (En[prop1, prop2])       = (En[prop1, prop2])
deMorgan (Of[prop1, prop2])       = (Of[prop1, prop2])
deMorgan (Niet proplijst)         = (Niet (deMorgan proplijst))
deMorgan (Bool a)                 = Bool a
deMorgan (Var a)                  = Var a
deMorgan (prop1 :-> prop2)        = (deMorgan prop1 :-> deMorgan prop2)




ontleed      :: String ->  Prop
ontleed ("True") = Bool True
ontleed (x:[])   =  Var [x]

-- (eqList eqBool) should be: eqBool 
-- 89,34-46
