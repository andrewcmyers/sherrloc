module PropLogic where

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
evalueer prop bedeling = case prop of
                         En   (eersteprop:volgendeprops) -> evalueer eersteprop bedeling && evalueer (head (volgendeprops)) bedeling
                         En   []                         -> False
                         Of   (eersteprop:volgendeprops) -> evalueer eersteprop bedeling || evalueer (head (volgendeprops)) bedeling
                         Of   []                         -> False
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
deMorgan prop = case prop of
                Niet(En(eersteprop:volgendeprops)) -> Of (deMorgan(Niet (eersteprop)) : (map deMorgan(map Niet (volgendeprops))))
                Niet(Of(eersteprop:volgendeprops)) -> En (deMorgan(Niet (eersteprop)) : (map deMorgan(map Niet (volgendeprops))))
                En(eersteprop:volgendeprops)       -> En (map deMorgan (eersteprop:volgendeprops))
                Of(eersteprop:volgendeprops)       -> Of (map deMorgan (eersteprop:volgendeprops))
                prop1 :-> prop2                    -> (deMorgan prop1) :-> (deMorgan prop2)
                Bool boolwaarde                    -> Bool boolwaarde
                Var string                         -> Var string
                Niet(Var string)                   -> Niet(Var string)
                En []                              -> False

testje :: String
testje =  toon(deMorgan (Niet (En [Of [Var "p", Var "q"],Of [Var "p", Var "q"]])))

-- False should be: Var "error"
-- 166,55-59
