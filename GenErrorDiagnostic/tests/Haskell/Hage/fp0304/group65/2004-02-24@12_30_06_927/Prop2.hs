module Prop2 where

import Data.List

data Prop
      = En [Prop]
      | Of [Prop]
      | Niet Prop
      | Prop :-> Prop
      | Var String
      | Bool Bool

type Bedeling = [String]

eqString      :: String -> String -> Bool 
eqString = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqChar      :: Char -> Char -> Bool
eqChar = undefined
showBool       :: Bool -> String
showBool = undefined

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
e2 p [] q     = (evalueer p []) && (evalueer q [])
e2 p (x:xs) q | (evalueer p x) && (evalueer q x) = e2 p xs q
              | otherwise = False









deMorgan :: Prop -> Prop
deMorgan (Var s)       = Var s
deMorgan (p :-> q)     = (deMorgan p) :-> (deMorgan q)
deMorgan (Of p)        | uitsluitendNiet (map deMorgan p)
                           = Niet (En (map deMorgan
                             (verwijderNiet (map deMorgan p))))
                       | otherwise
                             = Of (map deMorgan p)
deMorgan (En p)        | uitsluitendNiet (map deMorgan p)
                           = Niet (Of (map deMorgan
                             (verwijderNiet (map deMorgan p))))
                       | otherwise
                                = En (map deMorgan p)
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
toonHaakjes (p :-> q)   = "(" ++ (toon (p :-> q)) ++ ")"
toonHaakjes (Of (p:ps)) | (length ps)==0 = toon p
                        | otherwise      = "(" ++ (toon (Of (p:ps))) ++")"
toonHaakjes (Of [])     = ""
toonHaakjes (En (p:ps)) | (length ps)==0 = toon p
                        | otherwise      = "(" ++ (toon (En (p:ps))) ++")"
toonHaakjes p           = toon p










ontleed :: String -> Prop
ontleed s | ((zoekEind (length s) s ")")>(zoek 1 s " \\/ (")) && ((zoek 1 s " \\/ (")>0)
                                    = Of[(ontleed (left s ((zoek 1 s " \\/ (")-1))), (ontleed (cut s ((zoek 1 s " \\/ (")+5) (zoekEind (length s) s ")")))]
          | ((zoekEind (length s) s ")")>(zoek 1 s " /\\ (")) && ((zoek 1 s " /\\ (")>0)
                                    = En[(ontleed (left s ((zoek 1 s " /\\ (")-1))), (ontleed (cut s ((zoek 1 s " /\\ (")+5) (zoekEind (length s) s ")")))]
          | ((zoekEind (length s) s ")")>(zoek 1 s " -> (")) && ((zoek 1 s " -> (")>0)
                                    = (ontleed (left s ((zoek 1 s " -> ")-1))) :-> (ontleed (cut s ((zoek 1 s " -> (")+5) (zoekEind (length s) s ")")))
          | (zoek 1 s " /\\ ")>0    = En[(ontleed (left s ((zoek 1 s " /\\ ")-1))), (ontleed (right s ((length s)-(zoek 1 s " /\\ ")-3)))]
          | (zoek 1 s " \\/ ")>0    = Of[(ontleed (left s ((zoek 1 s " \\/ ")-1))), (ontleed (right s ((length s)-(zoek 1 s " \\/ ")-3)))]
          | (zoek 1 s " -> ")>0     = (ontleed (left s ((zoek 1 s " -> ")-1))) :-> (ontleed (right s ((length s)-(zoek 1 s " -> ")-3)))
          | eqString (left s 1) "!" = Niet (ontleed (right s ((length s)-1)))
          | eqString s "T"          = Bool True
          | eqString s "F"          = Bool False
          | otherwise               = Var (verwijderHaakjes 1 s)
          where verwijderHaakjes i t | i>(length t) = ""
                                     | (eqString (mid t i 1) "(") || (eqString (mid t i 1) ")")
                                                    = verwijderHaakjes (i+1) t
                                     | otherwise    = (mid t (i) 1) ++ (verwijderHaakjes (i+1) t)




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






overzicht :: Prop -> IO()
overzicht p = putStr ("Propositie:  " ++ (toon p) ++ (tekenTabel (getWidth (geefAlle p)) p))





              where getWidth []     = 0
                    getWidth (b:bs) | (length (unwords b))>(getWidth bs) = length (unwords b)
                                    | otherwise                = getWidth bs

tekenTabel :: Int -> Prop -> IO()
tekenTabel n p = "\n+-" ++ (replicate n '-') ++ "-+\n| True Vars | Value" ++
                 "|\n+-----------+-------+\n" ++
                 (tekenRijen p (geefAlle p)) ++
                 "+-----------+-------+\n"

tekenRijen :: Prop -> [Bedeling] -> String
tekenRijen _ []     = ""
tekenRijen p (b:bs) = "| " ++ (spaceRij 10 (unwords b)) ++
                          "| " ++ (spaceRij 6 (showBool
                                  (evalueer p b))) ++
                          "|" ++ "\n" ++
                          (tekenRijen p bs)

spaceRij :: Int -> String -> String
spaceRij i s = s ++ (replicate (i - (length s)) ' ')










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
testProp = (Var "p" :-> En[Niet(Var "q"), Var "r"])

testString :: [Bedeling]
testString = [["p","q","r"],["p","q"],["p","q"],["p"],["q"],["p"],[]]

taut :: Prop
taut = (Of [(Of [Var "p", Niet (Var "q")]), (Of [Bool False, Bool True])])

contr :: Prop
contr = (En [Var "p", Niet (Var "p")])

-- signature should be: Int -> Prop -> String
-- 328,15-33
