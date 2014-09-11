module Logica where
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
eqChar      :: Char -> Char -> Bool 
eqChar = undefined
isAlpha :: Char -> Bool
isAlpha = undefined

evalueer :: Prop -> Bedeling -> Bool
evalueer (Var x) bed = elemBy eqString x bed
evalueer (Niet p) bed = not(evalueer p bed)
evalueer (p :-> q) bed = evalueer (Niet p) bed || evalueer q bed
evalueer (Bool x) _ = x
evalueer (En lijst) bed = and (map (`evalueer` bed) lijst)
evalueer (Of lijst) bed = or (map (`evalueer` bed) lijst)


vervulbaar :: Prop -> [Bedeling]
vervulbaar propje = filter (evalueer propje) (subs (zoekvars propje))

zoekvars :: Prop -> Bedeling
zoekvars (Var x) = [x]
zoekvars (Niet p) = zoekvars p
zoekvars (p :-> q) = zoekvars p ++ zoekvars q
zoekvars (Bool _) = []
zoekvars (En lijst) = concat (map zoekvars lijst)
zoekvars (Of lijst) = concat (map zoekvars lijst)

subs :: [a] -> [[a]]
subs [] = [ [] ]
subs (k:s) = map (k:) subsxs ++ subsxs
        where subsxs = subs s




tautologie :: Prop -> Bool
tautologie propje = eqList (eqList eqString) (vervulbaar propje) (subs (zoekvars propje))



contradictie :: Prop -> Bool
contradictie propje = null(vervulbaar propje)



isSimpel :: Prop -> Bool
isSimpel (Var _) = True
isSimpel (Bool _) = True
isSimpel (Niet p) = isSimpel p
isSimpel _ = False

toon :: Prop -> String
toon (Var x) = x
toon (Niet p) | isSimpel p = "!" ++ toon p ++ ""
              | otherwise = "!(" ++ toon p ++ ")"
toon (p :-> q) | isSimpel p && isSimpel q = toon p ++ " -> " ++ toon q
               | isSimpel p = toon p ++ " -> " ++ "(" ++ toon q ++ ")"
               | isSimpel q = "(" ++ toon p ++ ")" ++ " -> " ++ toon q
               | otherwise = "(" ++ toon p ++ ")" ++ " -> " ++ "(" ++ toon q ++ ")"
toon (Bool x) | x = "T"
              | True = "F"

toon (En (k:[])) = toon k
toon (En (k:s)) | isSimpel k  = toon k ++ " /\\ " ++ toon (En s)
                | otherwise = "(" ++ toon k ++ ")" ++ " /\\ " ++ toon (En s)

toon (Of (k:[])) = toon k
toon (Of (k:s)) | isSimpel k = toon k ++ " \\/ " ++ toon (Of s)
                | otherwise = "(" ++ toon k ++ ")" ++ " \\/ " ++ toon (Of s)



equivalent :: Prop -> Prop -> Bool
equivalent propje1 propje2 = and( map (evalueer propje1) lijstje2)
                             && and ( map (evalueer propje2) lijstje1)
                                where lijstje1 = vervulbaar propje1
                                      lijstje2 = vervulbaar propje2





deMorgan :: Prop -> Prop
deMorgan (Var x) = (Var x)
deMorgan (Bool x) = (Bool x)
deMorgan (p :-> q) = ((deMorgan p) :-> (deMorgan q))
deMorgan (En propjes) = En(map deMorgan propjes)
deMorgan (Of propjes) = Of(map deMorgan propjes)
deMorgan (Niet(En propjes)) = (Of (map ontken propjes))
deMorgan (Niet(Of propjes)) = (En (map ontken propjes))
deMorgan (Niet p) = (Niet(deMorgan p))

ontken :: Prop -> Prop
ontken x = (Niet x)








ontleed1 :: String -> Prop
ontleed1 (k:s)     | isAlpha k = [k]
                   | eqChar '!' k = k:(ontleed1 s)
                   | otherwise = ontleed1 (zoekeerstea 1 s)

zoekeerstea :: Int -> String -> String

zoekeerstea n (k:s) | n==1 && eqChar k ')' = ""
                    | eqChar k ')'         = ")" ++ zoekeerstea (n-1) s
                    | eqChar k '('         = "(" ++ zoekeerstea (n+1) s
                    | otherwise            = (k: (zoekeerstea n s))

geefLit :: Char -> Prop
geefLit c | eqChar c 'T' = (Bool True)
          | eqChar c 'F' = (Bool False)
          | otherwise = (Var [c])

versimpelString :: String -> String
versimpelString [] = []
versimpelString (k:s) | eqChar k ' '     = versimpelString s
                      | eqChar k '-'     = '>' : versimpelString (drop 1 s)
                      | eqChar k '/'     = '^' : versimpelString (drop 1 s)
                      | eqChar k '\\'    = '|' : versimpelString (drop 1 s)
                      | otherwise = k : versimpelString s

-- signature should be: String -> String
-- 117,1-8  117,13-26   117,23-26
