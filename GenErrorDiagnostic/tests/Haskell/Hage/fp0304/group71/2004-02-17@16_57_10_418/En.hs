module En where

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
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

evalueer :: Prop -> Bedeling -> Bool

evalueer (Bool  p) _   = p
evalueer (Var   p) bed = elemBy eqString p bed
evalueer (Niet  p) bed = not (evalueer p bed)
evalueer (Of    p) bed = or  (map (flip evalueer bed) p)
evalueer (En    p) bed = and (map (flip evalueer bed) p)
evalueer (p :-> q) bed = (not (evalueer p bed)) || evalueer q bed




vervulbaar :: Prop -> [Bedeling]

vervulbaar p = filter (evalueer p) (subs(variabelen p))

variabelen :: Prop -> Bedeling

variabelen (Bool  _) = []
variabelen (Var   p) = [p]
variabelen (Niet  p) = variabelen p
variabelen (Of    p) = concat (map (variabelen) p)
variabelen (En    p) = concat (map (variabelen) p)
variabelen (p :-> q) = variabelen p ++ variabelen q


subs :: [a] -> [[a]]

subs []     = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
     where subsxs = subs xs



tautologie :: Prop -> Bool

tautologie p = eqList (eqList eqString) (vervulbaar p) (subs (variabelen p))




contradictie :: Prop -> Bool

contradictie = tautologie . Niet




toon :: Prop -> String

toon (Bool  p) | p         = "True"
               | otherwise = "False"
toon (Var   p) = p
toon (Niet p) = f p
        where   f r@(En _)    = tonen r
                f r@(Of _)    = tonen r
                f r@(_ :-> _) = tonen r
                f r           = "!" ++ (toon r)
toon (Of   p) = weergave " \\/ " (map f p)
        where   f (q :-> r) = "(" ++ toon (q :-> r) ++ ")"
                f q         = toon q
toon (En   p) = weergave " /\\ " (map f p)
        where   f (Of l)    = "(" ++ toon (Of l) ++ ")"
                f (q :-> r) = "(" ++ toon (q :-> r) ++ ")"
                f q         = toon q
toon (p :-> q) = toon p ++ " -> " ++ toon q

tonen :: Prop -> String
tonen p = "!(" ++ (toon p) ++ ")"


weergave :: String -> [String] -> String

weergave t s = take (length r - 4) r
                  where r = foldl (++) [] (map (++ t) s)



equivalent :: Prop -> Prop -> Bool

equivalent p q = eqList (eqList eqString) (vergelijk p) (vergelijk q)
           where vergelijk r = filter (evalueer r) (subs(variabelen (En [p,q])))




deMorgan :: Prop -> Prop

deMorgan (Bool  p)           = Bool p
deMorgan (Var   p)           = Var p
deMorgan (p :-> q)           = deMorgan (p) :-> deMorgan (q)
deMorgan (Niet (En(p:q:[]))) = Of [deMorgan (Niet p), deMorgan (Niet q)]
deMorgan (Niet (Of(p:q:[]))) = En [deMorgan (Niet p), deMorgan (Niet q)]
deMorgan (Niet p)            = Niet (deMorgan p)
deMorgan (En    p)           = En (map deMorgan p)
deMorgan (Of    p)           = Of (map deMorgan p)




ontleed :: String -> Prop
ontleed = zetOm.controle.vindOnderdelen


zetOm :: [String] -> Prop

zetOm []        = []
zetOm xss@(x:r) | length xss == 1 && eqString x "T"              = Bool True
                | length xss == 1 && eqString x "F"              = Bool False
                | length xss == 1 && eqString x "True"           = Bool True
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

controle xss = xss


vindOnderdelen :: String -> [String]

vindOnderdelen [] = []
vindOnderdelen (' ':xs) = vindOnderdelen xs
vindOnderdelen ('/':'\\':xs) = "/\\" : vindOnderdelen xs
vindOnderdelen ('\\':'/':xs) = "\\/" : vindOnderdelen xs
vindOnderdelen ('-':'>':xs) = "->" : vindOnderdelen xs
vindOnderdelen ('!':xs) = "!" : vindOnderdelen xs
vindOnderdelen t@('(':_) = snd3 splits : vindOnderdelen(thd3 splits)
        where splits = vindHaakje((0,"",t))
vindOnderdelen t@(_:_) = fst splits : vindOnderdelen(snd splits)
        where splits = vindWoord(("",t))


vindHaakje :: (Int,String,String) -> (Int,String,String)

vindHaakje (a,b,('(':c)) | a==0      = vindHaakje (a+1,b,c)
                         | otherwise = vindHaakje (a+1,b++"(",c)
vindHaakje (a,b,(')':c)) | a==1      = (a-1,b,c)
                         | otherwise = vindHaakje (a-1,b++")",c)
vindHaakje (a,b,(x:c))   = vindHaakje (a,b++[x],c)
vindHaakje (a,b,[])      = (a,b,[])

vindWoord :: (String,String) -> (String,String)

vindWoord (b,[])           = (b,[])
vindWoord (b,(' ':c))      = (b,c)
vindWoord (b,('/':'\\':c)) = (b,"/\\"++c)
vindWoord (b,('\\':'/':c)) = (b,"\\/"++c)
vindWoord (b,('-':'>':c))  = (b,"->"++c)
vindWoord (b,(')':c))      = (b,")"++c)
vindWoord (b,(x:c))        = vindWoord (b++[x],c)

snd3         :: (a,b,c) -> b
snd3 (_,y,_) = y

thd3         :: (a,b,c) -> c
thd3 (_,_,z) = z

-- []  should be: Var "ongeldige bewerking"
-- 130,19-20
