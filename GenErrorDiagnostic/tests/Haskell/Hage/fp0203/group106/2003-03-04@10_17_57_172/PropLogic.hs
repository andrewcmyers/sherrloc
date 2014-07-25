module PropLogic where
elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqList        :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
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
evalueer prop bedeling = case prop of
                         En   (eersteprop:volgendeprops) -> evalueer eersteprop bedeling && evalueer (head (volgendeprops)) bedeling
                         Of   (eersteprop:volgendeprops) -> evalueer eersteprop bedeling || evalueer (head (volgendeprops)) bedeling
                         Niet propositie                 -> not (evalueer propositie bedeling)
                         prop1 :-> prop2                 -> (not (evalueer prop1 bedeling)) || (evalueer prop2 bedeling)
                         Bool boolwaarde                 -> boolwaarde
                         Var string                      -> elemBy eqString string bedeling










alleVar :: Prop -> Bedeling
alleVar prop = delDubbel ( case prop of
                           Var variabele   -> [variabele]
                           Bool _          -> []
                           Niet propositie -> alleVar propositie
                           En propositie   -> concatMap alleVar propositie
                           Of propositie   -> concatMap alleVar propositie
                           prop1 :-> prop2 -> (alleVar prop1) ++ (alleVar prop2)
                         )
                where delDubbel []                             = []
                      delDubbel (eerstevar:volgendevars)
                                   | elemBy eqString eerstevar volgendevars = delDubbel volgendevars
                                   | otherwise                              = eerstevar : (delDubbel volgendevars)





alleCombinaties :: Bedeling -> [Bedeling]
alleCombinaties []                       = [[]]
alleCombinaties (eerstevar:volgendevars) = map (eerstevar:) (alleCombinaties volgendevars) ++ (alleCombinaties volgendevars)







vervulbaar :: Prop -> [Bedeling]
vervulbaar propositie = evalueerAlles propositie (alleCombinaties (alleVar propositie))
                        where evalueerAlles _ []                              = []
                              evalueerAlles prop (eerstebedel:volgendebedel)
                                            | evalueer prop eerstebedel = eerstebedel: (evalueerAlles prop volgendebedel)
                                            | otherwise                 = evalueerAlles prop volgendebedel











tautologie :: Prop -> Bool
tautologie propositie = eqList eqString (maakLijstPlat allevervulbaar) (maakLijstPlat allecombies)
                        where allecombies         = alleCombinaties (alleVar propositie)
                              allevervulbaar      = vervulbaar propositie
                              maakLijstPlat lijst = concat lijst






contradictie :: Prop -> Bool
contradictie propositie | null lijstvanbedelingen = True
                        | otherwise = False
                          where lijstvanbedelingen = vervulbaar propositie


testtoon1 :: IO ()
testtoon1 = putStr (toon (Niet (Of [En [Var "p",Var "q"], Var "r"])))
testtoon2 :: IO ()
testtoon2 = putStr (toon (Of [Var "p", Var "q"] :-> Var "tralala"))


toon :: Prop -> String
toon propositie = case propositie of
                  Var v   -> v
                  Bool b  -> if b then "True" else "False"
                  En prop -> if null (tail prop) then (toon (head prop)) ++ ")" else "(" ++ (toon (head prop)) ++ "/\\" ++ (toon (En (tail prop)))
                  Of prop -> if null (tail prop) then (toon (head prop)) ++ ")" else "(" ++ (toon (head prop)) ++ "\\/" ++ (toon (Of (tail prop)))
                  p :-> q -> (toon p) ++ "->" ++ (toon q)
                  Niet p  -> "!" ++ (toon p)























equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2 = foldl (&&) True (map checkBool (alleCombinaties (alleVar prop1)))
                 where checkBool bedeling = eqBool (evalueer prop1 bedeling)  (evalueer prop2 bedeling)



deMorgan :: Prop -> Prop
deMorgan [] = []
deMorgan (Niet(En(eersteprop:volgendeprops))) = deMorgan (Of ((Niet (deMorgan eersteprop)) : (map Niet (map deMorgan volgendeprops))))
deMorgan (Niet(Of(eersteprop:volgendeprops))) = deMorgan (En ((Niet (deMorgan eersteprop)) : (map Niet (map deMorgan volgendeprops))))
deMorgan (En(eersteprop:volgendeprops))       = En (map deMorgan (eersteprop:volgendeprops))
deMorgan (Of(eersteprop:volgendeprops))       = Of (map deMorgan (eersteprop:volgendeprops))
deMorgan (prop1 :-> prop2)                    = (deMorgan prop1) :-> (deMorgan prop2)
deMorgan (Bool boolwaarde)                    = (Bool boolwaarde)
deMorgan (Var string)                         = (Var string)

-- the definition at line 143 should be removed
-- 143,10-11    143,15-16
