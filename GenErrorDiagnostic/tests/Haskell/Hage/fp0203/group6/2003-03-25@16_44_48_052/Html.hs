module Html (module Pretty, module Html) where

import Pretty

type Tag = String
type Attribuut = (String,String)

data HTML = Tekst String
          | SingleTag Tag [Attribuut]
          | ComplexTag Tag [Attribuut] [HTML]
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

type Requirement = (String, String)

validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html | and (map (validate2 html "") reqs) = Nothing
                   | otherwise = Just (geefReq eqBool (zip reqs (map (validate2 html "") reqs)) False)

geefReq :: (Bool -> Bool -> Bool) -> [(Requirement, Bool)] -> Bool -> Requirement
geefReq _ ((x,y): zs) False
       | eqBool y False = x
       | otherwise = geefReq eqBool zs False

validate2 ::  HTML -> String -> Requirement -> Bool
validate2 (Tekst _ ) _ _ = True
validate2 (SingleTag kind _ ) ouder (o, k) | eqString k kind = eqString o ouder
                                           | otherwise       = True
validate2 (ComplexTag kind _ htmls) ouder (o, k) | eqString k kind = eqString o ouder
                                                 | otherwise       = and [ validate2 html kind (o,k) | html <- htmls ]

ppHTML :: HTML -> Doc
ppHTML (Tekst t)             = tekst t
ppHTML (SingleTag s a )      = [simpelString [(SingleTag s a )]]
ppHTML (ComplexTag c a htmls)  | simpel [(ComplexTag c a htmls)] = [simpelString [(ComplexTag c a htmls)]]
                               | otherwise = nietSimpel [(ComplexTag c a htmls)]

simpel :: [HTML] -> Bool
simpel [(ComplexTag _ _ html@(x:xs))] = (null html || length html == 1) && simpel x




simpelString :: [HTML] -> String
simpelString []          = ""
simpelString [(Tekst a)] = a
simpelString [(SingleTag s a)]         = "<" ++ s ++ (attribuutNaarString a) ++ ">"
simpelString [(ComplexTag c [] [])]     = "<" ++ c ++ ">" ++ "</" ++ c ++ ">"
simpelString [(ComplexTag c a [])]     = "<" ++ c ++ (attribuutNaarString a) ++ ">" ++ "</" ++ c ++ ">"
simpelString [(ComplexTag c a (x:xs))] = "<" ++ c ++ (attribuutNaarString a) ++ ">"
                                            ++ simpelString [x] ++ simpelString xs ++ "</" ++ c ++ ">"

nietSimpel :: [HTML] -> Doc
nietSimpel [(ComplexTag c a htmls)] = tekst ("<" ++ c ++ (attribuutNaarString a) ++ ">")
                                       <-> [spaties 1]
                                       <|> ppHTMLhulp htmls
                                       <-> tekst ("</" ++ c ++ ">")

ppHTMLhulp :: [HTML] -> Doc
ppHTMLhulp [] = []
ppHTMLhulp (x:xs) = ppHTML x <-> ppHTMLhulp xs

attribuutNaarString :: [Attribuut] -> String
attribuutNaarString [] = ""
attribuutNaarString ((a, b): rest) = " " ++ a ++ "=" ++ "\"" ++ b ++ "\"" ++(attribuutNaarString rest)

-- x should be: [x]
-- 41,83-83
