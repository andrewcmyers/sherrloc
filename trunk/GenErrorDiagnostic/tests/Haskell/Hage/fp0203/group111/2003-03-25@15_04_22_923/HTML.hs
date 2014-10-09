module HTML(module Pretty, module HTML) where
import Pretty

showInt       :: Int -> String
showInt = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
data HTML
 = Tekst String
 | Enkel String [Attribute]
 | Dubbel String [Attribute] [HTML]


type Attribute = (String, String)
type Requirement = (String, String)






validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqlijst htmlijst | and (allReqs reqlijst htmlijst) = Nothing
                           | otherwise = Just (filter (not.f) reqlijst)
                                       where f x = test x "" htmlijst




allReqs :: [Requirement] -> HTML -> [Bool]
allReqs [] _ = []
allReqs (x:xs) htmlijst = test x "" htmlijst : allReqs xs htmlijst



test :: Requirement -> String -> HTML -> Bool
test (_,_) _ (Tekst _) = True
test (parent,child) parenttag (Enkel childtag _) | eqString child childtag = eqString parent parenttag
                                                 | otherwise = True
test (parent,child) parenttag (Dubbel childtag _ htmlijst) | eqString child childtag = eqString parent parenttag
                                                           | otherwise = and (map (test (parent,child) childtag) htmlijst)







ppHTML :: HTML -> Doc
ppHTML (Tekst string) = tekst string
ppHTML (Enkel string []) = tekst ("<" ++ string ++ ">")
ppHTML (Enkel string attr) = tekst ("<" ++ string ++ concatMap ppAttr attr ++ ">")
ppHTML (Dubbel string [] prop) | simpel prop = tekst ("<" ++ string ++ ">") <|> foldr (<|>) leeg (map ppHTML prop) <|> tekst ("</" ++ string ++ ">")
                               | not (simpel prop) = tekst ("<" ++ string ++ ">") <-> springIn 1 (foldr (<->) leeg (map ppHTML prop)) <-> tekst ("</" ++ string ++ ">")
ppHTML (Dubbel string attr prop) | simpel prop = tekst ("<" ++ string ++ concatMap ppAttr attr ++ ">") <|> foldr (<|>) leeg (map ppHTML prop) <|> tekst ("</" ++ string ++ ">")
                                 | not (simpel prop) = tekst ("<" ++ string ++ concatMap ppAttr attr ++ ">") <-> springIn 1 (foldr (<->) leeg (map ppHTML prop)) <-> tekst ("</" ++ string ++ ">")

ppAttr :: ([Char], [Char]) -> [Char]
ppAttr (str1,str2) = " " ++ str1 ++ "=\"" ++ str2 ++ "\""



simpel :: [HTML] -> Bool
simpel htmlijst = foldr (&&) True (map simpel1 htmlijst)

simpel1 :: HTML -> Bool
simpel1 (Tekst string) = True
simpel1 (Enkel string a) = True
simpel1 (Dubbel string a [])  = False
simpel1 (Dubbel string a [html]) = simpel1 html
simpel1 (Dubbel string a html) = False








color :: Int -> Int -> Int -> String
color x y z = "#" ++ numTohex x ++ numTohex y ++ numTohex z
numTohex :: Int -> [Char]
numTohex x   | ((x/16) < 10) && (rem x 16) < 10 = showInt (x /16)   ++ showInt (rem x 16)
             | ((x/16) < 10) && (rem x 16) >  9 = showInt (x/16)    ++ [chr ((rem x 16) + 55)]
             | ((x/16) >  9) && (rem x 16) < 10 = chr ((x/16) + 55) :  showInt (rem x 16)
             | ((x/16) >  9) && (rem x 16) >  9 = chr ((x/16) + 55) : [chr ((rem x 16) + 55)]


colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable lijst = Dubbel "TABLE" [("WIDTH","400"),("HEIGHT","400")] (rij 0 lijst)

rij :: Int -> [[(Int, Int, Int)]] -> [HTML]
rij r lijst | r == (length lijst) = []
            | r <  (length lijst) = (Dubbel "TR" [] (kolom (r,0) lijst)) : (rij (r+1) lijst)

kolom :: (Int, Int) -> [[(Int, Int, Int)]] -> [HTML]
kolom (r,k) lijst | k == (length lijst) = []
                  | k <  (length lijst) = (Dubbel "TD" [("BGCOLOR",(color x y z))] []) : (kolom (r,(k+1)) lijst)
                                        where (x,y,z) = ((lijst !! r) !! k)


ul :: [[HTML]] -> HTML
ul htmlijst = Dubbel "UL" [] (elementen 0 htmlijst)

elementen :: Int -> [[HTML]] -> [HTML]
elementen x htmlijst | x == (length htmlijst) = []
                     | x <  (length htmlijst) = (Dubbel "LI" [] (htmlijst !! x)) : elementen (x+1) htmlijst


h :: Int -> String -> HTML
h x string = Dubbel ("H"++[(chr(48+x))]) [] [Tekst string]


font :: [Attribute] -> [HTML] -> HTML
font attrlijst htmlijst = Dubbel "FONT" attrlijst htmlijst


text :: String -> HTML
text string = Tekst string


document :: String -> [HTML] -> HTML
document title content = Dubbel "HTML" [] [Dubbel "TITLE" [] [Tekst title], Dubbel "BODY" [] content]






kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel" [
                                     h 1 "Kleurentabel",
                                     ul [
                                         [
                                         font [("COLOR","#FF0000")] [Tekst "Rood"],
                                         Tekst " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                                         ],
                                         [
                                         font [("COLOR","#00FF00")] [Tekst "Groen"],
                                         Tekst " loopt van links naar rechts van 0 tot en met 250 in stappen van 25"
                                         ],
                                         [
                                         font [("COLOR","#0000FF")] [Tekst "Blauw"],
                                         Tekst " is overal 0"
                                         ]
                                        ],
                                      colorTable (genereer (0,0) (0,0,0))
                                     ]



genereer :: (Int, Int) -> (Int, Int, a) -> [[(Int, Int, a)]]
genereer (x,y) (r,g,b) | x == 11 = []
                       | x < 11 = gkol (x,y) (r,g,b) : genereer (x+1,y) (r+25,g,b)

gkol :: (Int, Int) -> (b, Int, c) -> [(b, Int, c)]
gkol (x,y) (r,g,b) | y == 11 = []
                   | y < 11 = (r,g,b) : gkol (x,y+1) (r,g+25,b)


main :: IO ()
main = layout (ppHTML kleurenTabel)



testcase :: HTML
testcase = (Dubbel "UL" [] [
                        Enkel "HR" [("WIDTH","50%")]
                        ,
                        Dubbel "LI" [] [
                                        Dubbel "B" [] [
                                                       Tekst "1"
                                                      ],
                                                       Tekst "12"
                                       ],
                        Dubbel "LI" [][
                                       Tekst "3"
                                      ]
                        ]
       )
testcase2 = Dubbel "UL" [] [Enkel "HR" [("WIDTH","50%")],Dubbel "LI" [] [Dubbel "B" [] [Tekst "1"],Tekst "12"],Dubbel "LI" [][Tekst "3"]]

-- should be head(filter ...)
-- 26,48-70   26,42-71
