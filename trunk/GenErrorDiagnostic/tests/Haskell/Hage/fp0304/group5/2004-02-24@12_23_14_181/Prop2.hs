module Prop2 where

import Data.List

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

data Prop
      = En [Prop]
      | Of [Prop]
      | Niet Prop
      | Prop :-> Prop
      | Var String
      | Bool Bool

type Bedeling = [String]










evalueer :: Prop -> Bedeling -> Bool
evalueer (Bool b) _ = b
evalueer (Var _) [] = False
evalueer (Var s) (x:xs) | eqString s x = True
                        | otherwise = evalueer (Var s) xs
evalueer (En (p:ps)) xs = evalueer p xs && evalueer (En ps) xs
evalueer (En _ ) _ = True
evalueer (Of (p:ps)) xs = evalueer p xs || evalueer (Of ps) xs
evalueer (Of _ ) _ = False
evalueer (Niet ps) xs = not (evalueer ps xs)
evalueer (p :-> q) xs = not (evalueer p xs) || evalueer q xs










vervulbaar :: Prop -> [Bedeling]
vervulbaar prop = filter (evalueer prop) (geefAlle prop)







geefAlle :: Prop -> [Bedeling]
geefAlle = subs . (nubBy (eqList eqChar)) . geefVars



geefVars :: Prop -> Bedeling
geefVars (Var s) = [s]
geefVars (Bool _) = [""]
geefVars (p :-> q) = geefVars p ++ geefVars q
geefVars (En (p:ps)) = geefVars p ++ geefVars (En ps)
geefVars (Of (p:ps)) = (geefVars p) ++ (geefVars (Of ps))
geefVars (Niet p) = geefVars p
geefVars _ = []




subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) subsxs ++ subsxs
                  where subsxs = subs xs


















tautologie :: Prop -> Bool
tautologie p = t2 p (geefAlle p)




t2 :: Prop -> [Bedeling] -> Bool
t2 p [] = evalueer p []
t2 p (x:xs) | evalueer p x = t2 p xs
            | otherwise = False









contradictie :: Prop -> Bool
contradictie p = c2 p (geefAlle p)




c2 :: Prop -> [Bedeling] -> Bool
c2 p [] = not (evalueer p [])
c2 p (x:xs) | evalueer p x = False
            | otherwise = c2 p xs










equivalent :: Prop -> Prop -> Bool
equivalent p q = (e2 p (geefAlle p) q) && (e2 p (geefAlle q) q)





e2 :: Prop -> [Bedeling] -> Prop -> Bool
e2 p [] q     = eqBool (evalueer p []) (evalueer q [])
e2 p (x:xs) q | eqBool (evalueer p x) (evalueer q x) = e2 p xs q
              | otherwise = False









deMorgan :: Prop -> Prop
deMorgan (Var s)       = Var s
deMorgan (p :-> q)     = (deMorgan p) :-> (deMorgan q)
deMorgan (Of p)        | uitsluitendNiet (map deMorgan p) = Niet (En (map deMorgan (verwijderNiet (map deMorgan p))))
                       | otherwise         = Of (map deMorgan p)
deMorgan (En p)        | uitsluitendNiet (map deMorgan p) = Niet (Of (map deMorgan (verwijderNiet (map deMorgan p))))
                       | otherwise         = En (map deMorgan p)
deMorgan (Niet (En p)) = Of (map deMorgan (map Niet p))
deMorgan (Niet (Of p)) = En (map deMorgan (map Niet p))
deMorgan (Niet p)      = Niet (deMorgan p)
deMorgan (Bool b)      = Bool b




uitsluitendNiet :: [Prop] -> Bool
uitsluitendNiet []            = True
uitsluitendNiet ((Niet _):ps) = uitsluitendNiet ps
uitsluitendNiet (_)           = False




verwijderNiet :: [Prop] -> [Prop]
verwijderNiet []            = []
verwijderNiet ((Niet p):ps) = p : (verwijderNiet ps)
verwijderNiet (p:ps)        = p : (verwijderNiet ps)













toon :: Prop -> String
toon (Var s)     = s
toon (p :-> q)   = (toonHaakjes p) ++ " -> " ++ (toonHaakjes q)
toon (Of (p:ps)) | (length ps)==0 = toon p
                 | bevatIm p      = (toonHaakjes p) ++ " \\/ " ++ (toon(En ps))
                 | otherwise      = (toon p) ++ " \\/ " ++ (toon (Of ps))
toon (Of [])     = ""
toon (En (p:ps)) | (length ps)==0 && (bevatOf p) = toonHaakjes p
                 | (length ps)==0 = toon p
                 | bevatOf p || bevatIm p
                                  = (toonHaakjes p) ++ " /\\ " ++ (toon(En ps))
                 | otherwise      = (toon p) ++ " /\\ " ++ (toon (En ps))
toon (En [])     = ""
toon (Niet p)    = "!" ++ (toonHaakjes p) ++ ""
toon (Bool True) = "T"
toon (Bool False)= "F"




bevatOf :: Prop -> Bool
bevatOf (Of _) = True
bevatOf _      = False



bevatIm :: Prop -> Bool
bevatIm (_ :-> _) = True
bevatIm _         = False



toonHaakjes :: Prop -> String
toonHaakjes (Var s)     = s
toonHaakjes (p :-> q)   = "(" ++ (toon (p :-> q)) ++ ")"
toonHaakjes (Of (p:ps)) | (length ps)==0 = toon p
                        | otherwise      = "(" ++ (toon (Of (p:ps))) ++")"
toonHaakjes (Of [])     = ""
toonHaakjes (En (p:ps)) | (length ps)==0 = toon p
                        | otherwise      = "(" ++ (toon (En (p:ps))) ++")"
toonHaakjes (En [])     = ""
toonHaakjes (Niet p)    = "!" ++ (toonHaakjes p) ++ ""
toonHaakjes (Bool True) = "T"
toonHaakjes (Bool False)= "F"











ontleed :: String -> Prop

ontleed s | eqString s "T"          = Bool True
          | eqString s "F"          = Bool False
          | eqString s (zoekHaakjes s) = ontleed2 s
          | otherwise = Var (verwijderHaakjes 1 s)
                where verwijderHaakjes i t | i>(length t) = ""
                                           | (eqString (mid t i 1) "(") || (eqString (mid t i 1) ")")
                                                          = verwijderHaakjes (i+1) t
                                           | otherwise    = (mid t (i) 1) ++ (verwijderHaakjes (i+1) t)

ontleed2 :: String -> Prop
ontleed2 s | (zoek 1 s " /\\ ")>0
                       = En[(ontleed (zoekHaakjes (left s ((zoek 1 s " /\\ ")-1)))), (ontleed (zoekHaakjes (right s ((length s)-(zoek 1 s " /\\ ")-3))))]
           | (zoek 1 s " \\/ ")>0
                       = Of[(ontleed (zoekHaakjes (left s ((zoek 1 s " \\/ ")-1)))), (ontleed (zoekHaakjes (right s ((length s)-(zoek 1 s " \\/ ")-3))))]
           | (zoek 1 s " -> ")>0
                       = (ontleed (zoekHaakjes ((left s ((zoek 1 s " -> ")-1)))) :-> (ontleed (zoekHaakjes (right s ((length s)-(zoek 1 s " -> ")-3)))))
           | eqString (left s 1) "!"
                       = Niet (ontleed (zoekHaakjes (right s ((length s)-1))))
           | otherwise = "blabalbla "++s++"blablabal"






















zoekHaakjes :: String -> String
zoekHaakjes s = zhaak s 0 0 ""

zhaak :: String -> Int -> Int -> String -> String
zhaak [] _ _ s = s
zhaak (x:xs) ho hs s | ho == hs && ho>0 = s
                     | eqChar x '(' = if ho==0  then zhaak xs (ho+1) hs s
                                      else           zhaak xs (ho+1) hs (s++"(")
                     | eqChar x ')' = if ho==(hs+1) then zhaak xs ho (hs+1) s
                                      else               zhaak xs ho (hs+1) (s++")")
                     | ho > hs      = zhaak xs ho hs (s++[x])
                     | otherwise    = zhaak xs ho hs s




zoek :: Int -> String -> String -> Int
zoek i s t | i-1>(length s)-(length t)       = 0
           | eqString (mid s i (length t)) t = i
           | otherwise                       = zoek (i+1) s t




zoekEind :: Int -> String -> String -> Int
zoekEind i s t | i<=0                            = 0
               | eqString (mid s i (length t)) t = i-1
               | otherwise                       = zoek (i-1) s t




left :: String -> Int -> String
left [] _     = ""
left (s:ss) i | i>0       = s : (left ss (i-1))
              | otherwise = left ss (i-1)




right :: String -> Int -> String
right []     _ = ""
right (s:ss) i | (length ss)<=i-1  = s : (right ss (i))
               | otherwise         = right ss (i)




mid :: String -> Int -> Int -> String
mid s i l = right (left s (i+l-1)) l




cut :: String -> Int -> Int -> String
cut s i l = mid s i (l-i)









about :: IO()
about = do putStr ("/------------------------------------\\\n" ++
                   "|                                    |\n" ++
                   "|     [functioneel programmeren]     |\n" ++
                   "|                                    |\n" ++
                   "|     MATHIJS LAGERBERG   #0301523   |\n" ++
                   "|            and                     |\n" ++
                   "|      PAUL LAMMERTSMA    #0305235   |\n" ++
                   "|                                    |\n" ++
                   "|          . . present . .           |\n" ++
                   "|                                    |\n" ++
                   "|             -- the --              |\n" ++
                   "|     PROPOSITION THINGAMABOBBER     |\n" ++
                   "|                 ~                  |\n" ++
                   "|                                    |\n" ++
                   "\\------------------------------------/\n")



testProp :: Prop
testProp = (Var "p" :-> Var "q")

testString :: [Bedeling]
testString = [["p","q","r"],["p","q"],["p","q"],["p"],["q"],["p"],[]]

taut :: Prop
taut = (Of [(Of [Var "p", Niet (Var "q")]), (Of [Bool False, Bool True])])

contr :: Prop
contr = (En [Var "p", Niet (Var "p")])

-- should be: Var (...)
-- 278,26-53
