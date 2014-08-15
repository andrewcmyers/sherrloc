module HTML(module Pretty, module HTML) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined

eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

data Html
  = Tekst String
  | Enkeltag String [Attribuut]
  | Dubbeltag String [Attribuut] [Html]

type Attribuut = (String, String)






validate :: [Requirement] -> Html -> Maybe Requirement
type Requirement = (String, String)

validate rec html | and (map validate' rec )= Nothing
                  | otherwise = Just (rec !! (length (takeWhile (eqBool False) (map validate' rec))))
                                    where validate' recu = validate2 recu html

validate2 :: Requirement -> Html -> Bool
validate2 (ouderrec,kindrec) html = validate2' (ouderrec,kindrec) html ""

validate2' :: Requirement -> Html -> String -> Bool
validate2' _                  (Tekst _) _             = True
validate2' (ouderrec,kindrec) (Enkeltag naam _) ouder | eqString kindrec naam = eqString ouderrec ouder
                                                      | otherwise = True
validate2' (ouderrec,kindrec) (Dubbeltag naam _ html) ouder | eqString kindrec naam = eqString ouderrec ouder
                                                            | otherwise = and(map validate3 html)
                                                                where validate3 html2 = validate2' (ouderrec,kindrec) html2 naam







simpel :: Html -> Doc
simpel (Tekst regel) = [regel]
simpel (Enkeltag naam lijst) = ["<"++naam++" "++(atr lijst)++">"]

atr :: [Attribuut] -> String
atr [] = ""
atr (x:xs) = atr2 x ++ atr xs

atr2 :: Attribuut -> String
atr2 (s1,s2) = s1 ++ " = " ++ s2 ""

-- missing "++" between s2 and ""
-- 55,31-35
