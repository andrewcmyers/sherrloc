module Html (module Pretty, module Html) where
import Pretty

eqString      :: String -> String -> Bool 
eqString = undefined
showInt       :: Int -> String
showInt = undefined


data HTML = Tekst String
          | TagSingle String [Attribute]
          | Tag String [Attribute] [HTML]

type Requirement = (String, String)
type Attribute = (String, String)










voorbeeld :: HTML
voorbeeld = Tag "UL" [] [    Tag "LI" [] [ Tag "FONT" [("COLOR","#0000FF")] [Tekst "Eerste"],
                                           Tekst "punt"
                                         ]                                                     ,
                             TagSingle "HR" []                                                 ,
                             Tag "LI" [] [Tekst "Tweede punt"]
                        ]







validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (x:xs) html | hulpValidate x html "" = Just x
                     | otherwise              = validate xs html







hulpValidate :: Requirement -> HTML -> String -> Bool
hulpValidate _ (Tekst _) _ = False
hulpValidate (req1,req2) (TagSingle naam _) ouder | (eqString req2 naam) = not(eqString ouder req1)
                                                  | otherwise            = False
hulpValidate (req1,req2) (Tag naam _ html) ouder  | (eqString req2 naam) = not(eqString ouder req1)
                                                  | otherwise            = (or . ( map hulpValidate')) html
                                                  where hulpValidate' htmlkind = hulpValidate (req1,req2) htmlkind naam









ppHTML :: HTML -> Doc
ppHTML html | simpel html = tekst(omzetten html)
            | otherwise = hulpHTML html


hulpHTML :: HTML -> Doc
hulpHTML (Tag naam attrl [])   = tekst ("<" ++ naam ++ " " ++ (attributen attrl) ++ ">") <-> tekst ("</" ++ naam ++ ">")
hulpHTML (Tag naam attrl html) = tekst ("<" ++ naam ++ " " ++ (attributen attrl) ++ ">") <-> (map ppHTML html) <-> tekst ("</" ++ naam ++ ">")


simpel :: HTML -> Bool
simpel (Tekst _) = True
simpel (TagSingle _ _) = True
simpel (Tag _ _ html) | length html == 1 && and(map simpel html) = True
                      | otherwise = False
simpel (Tag _ _ []) = False




omzetten :: HTML -> String
omzetten (Tekst string)         = string
omzetten (TagSingle naam lijst) = "<" ++ naam ++ " " ++ (attributen lijst) ++ ">"
omzetten (Tag naam lijst [])    = "<" ++ naam ++ " " ++ (attributen lijst) ++ ">" ++ "</" ++ naam ++ ">"
omzetten (Tag naam lijst html)  = "<" ++ naam ++ " " ++ (attributen lijst) ++ ">" ++ restHTML ++ "</" ++ naam ++ ">"
                                where restHTML = (concatMap omzetten html)







attributen :: [Attribute] -> String
attributen lijst = init(concatMap (koppel) lijst)
                 where koppel (a,w) = a ++ "=" ++ "\"" ++ w++ "\"" ++ " "
attributen [] = ""





color :: Int -> Int -> Int -> String
color rood groen blauw = "#" ++ combo (getalKiezen div) (getalKiezen rem)
                      where roodgroenblauw = rood:groen:blauw:[]
                            lijst = "0123456789ABCDEF"
                            getalKiezen functie = map (lijst !!) (map (`functie` 16) roodgroenblauw)






combo :: [a] -> [a] -> [a]
combo lijst1 [] = lijst1
combo [] lijst2 = lijst2
combo (x:xs) (y:ys) = x:(y:(combo xs ys))

























h :: Int -> String -> HTML
h waarde string = Tag grootte [] [Tekst string]
                where grootte = "H" ++ (showInt waarde)





font :: [Attribute] -> [HTML] -> HTML
font [fontAttributen] [html] = Tag "FONT" [fontAttributen] [html]





text :: String -> HTML
text string = Tekst string






document :: String -> [HTML] -> HTML
document title inhoudBody = Tag "HTML" [] [ Tag "HEAD" [] [ Tag "TITLE" [] [Tekst title] ],
                                            Tag "BODY" [] inhoudBody
                                          ]




















test2 :: HTML
test2 = Tag "HTML" [] [    Tag "UL" [] [ Tag "BODY" [("COLOR","#0000FF")] [Tekst "Eerste"],
                                         Tekst "punt"
                                         ]                                                     ,
                           TagSingle "HR" []                                                 ,
                           Tag "LI" [] [Tekst "Tweede punt"]
                        ]

test3 :: HTML
test3 = Tag "HTML" [] [ Tag "HEAD" [] [],
                        Tag "BODY" [] []
                      ]

test4 :: HTML
test4 = Tag "BODY" [] [ Tag "HTML" [] [],
                        Tag "HEAD" [] [  Tag "UL" [] [    Tag "LI" [] [ Tag "FONT" [("COLOR","#0000FF")] [Tekst "Eerste"],
                                                                        Tekst "punt"
                                                                      ]                                                     ,
                                                          TagSingle "HR" []                                                 ,
                                                          Tag "LI" [] [Tekst "Tweede punt"]
                                                    ]
                        ]
                      ]

-- map should be: concatMap
-- 74,95-109
