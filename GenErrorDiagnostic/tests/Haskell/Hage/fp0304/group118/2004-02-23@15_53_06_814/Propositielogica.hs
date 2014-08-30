module Propositielogica where

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
eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined



evalueer :: Prop -> Bedeling -> Bool

evalueer (En        lijst) bed = and (map (flip evalueer bed) lijst)
evalueer (Of        lijst) bed = or  (map (flip evalueer bed) lijst)
evalueer (Niet      prop ) bed = not (evalueer prop bed)
evalueer (prop1 :-> prop2) bed = (not (evalueer prop1 bed)) || evalueer prop2 bed
evalueer (Var       varia) bed = elemBy eqString varia bed
evalueer (Bool      bool ) _   = bool










vervulbaar :: Prop -> [Bedeling]

vervulbaar prop = filter (evalueer prop) (subs(variabelen prop))












variabelen :: Prop -> Bedeling

variabelen (Bool      _    ) = []
variabelen (Var       varia) = [varia]
variabelen (prop1 :-> prop2) = dubbeleEruit (variabelen prop1 ++ variabelen prop2)
variabelen (Niet      prop ) = dubbeleEruit (variabelen prop)
variabelen (En        lijst) = dubbeleEruit (concatMap variabelen lijst)
variabelen (Of        lijst) = dubbeleEruit (concatMap variabelen lijst)

dubbeleEruit :: Bedeling -> Bedeling

dubbeleEruit (x:xs) |elemBy eqString x xs = dubbeleEruit xs
                    |otherwise            = x ++ dubbeleEruit xs
dubbeleEruit []                           = []



subs :: [a] -> [[a]]

subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
        where subsxs = subs xs








tautologie :: Prop -> Bool

tautologie prop = eqList (eqList eqString) (vervulbaar prop) (subs (variabelen prop))








contradictie :: Prop -> Bool

contradictie = tautologie . Niet




















toon :: Prop -> String

toon (En   prop)                          = weergave " /\\ " (map functie prop)
        where   functie (Of lijst)        = "(" ++ toon (Of lijst) ++ ")"
                functie (prop1 :-> prop2) = "(" ++ toon (prop1 :-> prop2) ++ ")"
                functie rest              = toon rest
toon (Of   prop)                          = weergave " \\/ " (map functie prop)
        where   functie (prop1 :-> prop2) = "(" ++ toon (prop1 :-> prop2) ++ ")"
                functie rest              = toon rest
toon (Niet   prop)                        = functie prop
        where   functie (En _)            = tonen prop
                functie (Of _)            = tonen prop
                functie (_ :-> _)         = tonen prop
                functie  _                = "!" ++ (toon prop)
toon (prop1 :-> prop2)                    = toon prop1 ++ " -> " ++ functie prop2
        where   functie prop@(_ :-> _)    = "(" ++ toon prop ++ ")"
                functie rest              = toon rest
toon (Var   varia)                        = varia
toon (Bool   bool) | bool                 = "True"
                   | otherwise            = "False"




tonen :: Prop -> String
tonen prop = "!(" ++ (toon prop) ++ ")"





weergave :: String -> [String] -> String

weergave teken (x:xs) = x ++ (concatMap (teken ++) xs)
weergave _     []     = []









equivalent :: Prop -> Prop -> Bool

equivalent prop1 prop2 = eqList (eqList eqString) (vergelijk prop1) (vergelijk prop2)
        where vergelijk prop = filter (evalueer prop) (subs(variabelen (En [prop1,prop2])))

















deMorgan :: Prop -> Prop

deMorgan (En        lijst)   = En (map deMorgan lijst)
deMorgan (Of        lijst)   = Of (map deMorgan lijst)
deMorgan (Niet (En lijst))   = Of (map (deMorgan.Niet) lijst)
deMorgan (Niet (Of lijst))   = En (map (deMorgan.Niet) lijst)
deMorgan (Niet       prop)   = Niet (deMorgan prop)
deMorgan (prop1 :-> prop2)   = (deMorgan prop1) :-> (deMorgan prop2)
deMorgan (Var       varia)   = Var varia
deMorgan (Bool       bool)   = Bool bool



ontleed :: String -> Prop
ontleed = zetOm.controle.vindOnderdelen



zetOm :: [String] -> Prop

zetOm []        = Var "ongeldige bewerking"
zetOm xss@(x:r) | length xss == 1 && eqString x "True"           = Bool True
                | length xss == 1 && eqString x "False"          = Bool False
                | length xss == 1 && eqChar '(' (head(x))        = ontleed (init(tail(x)))
                | length xss == 1                                = Var x
                | length xss == 2                                = Niet (ontleed (head r))
                | length xss == 3 && elemBy eqString "->" xss    = (ontleed x) :-> (ontleed (last r))
                | length xss >  2 && elemBy eqString "/\\" xss   = En (map ontleed (pakVars xss))
                | otherwise                                      = Of (map ontleed (pakVars xss))




pakVars        :: [String] -> [String]

pakVars []     = []
pakVars (x:xs) | eqString x "/\\" || eqString x "\\/" = pakVars xs
               | otherwise = x : pakVars xs








controle :: [String] -> [String]

controle xss | length xss < 3            = xss
             | elemBy eqString "!" xss   = controle(verwerkNieten(reverse xss))
             | length xss < 4            = xss
             | checkOps                  = xss
             | elemBy eqString "/\\" xss = controle(verwerkEnOf "/\\" xss)
             | elemBy eqString "\\/" xss = controle(verwerkEnOf "\\/" xss)
             | otherwise                 = verwerkImplicaties(reverse xss)
        where verwerkNieten []         = []
              verwerkNieten (x:[])     = [x]
              verwerkNieten (x:"!":[]) = ["(!"++x++")"]
              verwerkNieten (x:y:[])   = [y,x]
              verwerkNieten (x:"!":xs) = verwerkNieten xs ++ ["(!"++x++")"]
              verwerkNieten (x:y:xs)   = verwerkNieten (y:xs) ++ [x]
              verwerkImplicaties [] = []
              verwerkImplicaties (x:[]) = [x]
              verwerkImplicaties (x:y:[]) = [y,x]
              verwerkImplicaties (x:"->":xs) = ["(" ++ concat (reverse xs) ++ ")","->",x]
              verwerkImplicaties (x:y:xs) = verwerkImplicaties (y:xs) ++ [x]
              verwerkEnOf _ []             = []
              verwerkEnOf _ (x:[])         = [x]
              verwerkEnOf _ (x:y:[])       = [x,y]
              verwerkEnOf eo (x:y:z:[])    | eqString y eo = [concat ["(",x,y,z,")"]]
                                           | otherwise     = [x,y,z]
              verwerkEnOf eo (x:y:z:zz:xs) | eqString zz eo && eqString y eo = verwerkEnOf eo ((x++y++z):zz:xs)
                                           | eqString zz eo = x : y : verwerkEnOf eo (z:zz:xs)
                                           | eqString y eo  = [concat ["(",x,y,z,")"]] ++ verwerkEnOf eo (zz:xs)
                                           | otherwise      = x : y : verwerkEnOf eo (z:zz:xs)
              checkOps | checkEn && checkOf || checkEn && checkIm || checkOf && checkIm = False
                       | checkImps > 1 = False
                       | otherwise = True
                where checkEn = elemBy eqString "/\\" xss
                      checkOf = elemBy eqString "\\/" xss
                      checkIm = elemBy eqString "->" xss
                      checkImps = length(filter (eqString "->") xss)




vindOnderdelen :: String -> [String]

vindOnderdelen []            = []
vindOnderdelen (' ':xs)      = vindOnderdelen xs
vindOnderdelen ('/':'\\':xs) = "/\\" : vindOnderdelen xs
vindOnderdelen ('\\':'/':xs) = "\\/" : vindOnderdelen xs
vindOnderdelen ('-':'>':xs)  = "->" : vindOnderdelen xs
vindOnderdelen ('!':xs)      = "!" : vindOnderdelen xs
vindOnderdelen ('(':x)       = snd3 splits : vindOnderdelen(thd3 splits)
        where splits = vindHaakje((1,"(",x))
              snd3         :: (a,b,c) -> b
              snd3 (_,y,_) = y
              thd3         :: (a,b,c) -> c
              thd3 (_,_,z) = z
vindOnderdelen t@(_:_)       = fst splits : vindOnderdelen(snd splits)
        where splits = vindWoord(("",t))




vindHaakje :: (Int,String,String) -> (Int,String,String)

vindHaakje (a,b,('(':c)) = vindHaakje (a+1,b++"(",c)
vindHaakje (a,b,(')':c)) | a==1      = (a-1,b++")",c)
                         | otherwise = vindHaakje (a-1,b++")",c)
vindHaakje (a,b,(x:c))   = vindHaakje (a,b++[x],c)
vindHaakje (a,b,[])      = (a,b,[])





vindWoord :: (String,String) -> (String,String)

vindWoord (b,[])           = (b,[])
vindWoord (b,(' ':c))      = (b,c)
vindWoord (b,('/':'\\':c)) = (b,"/\\"++c)
vindWoord (b,('\\':'/':c)) = (b,"\\/"++c)
vindWoord (b,('-':'>':c))  = (b,"->"++c)
vindWoord (b,(x:c))        = vindWoord (b++[x],c)

-- ++ should be :
-- 69,47-48
