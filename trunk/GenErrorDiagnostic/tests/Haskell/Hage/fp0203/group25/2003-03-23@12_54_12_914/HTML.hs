module HTML(module Pretty, module HTML) where

import Pretty
import Data.Char

type Attr = (String, String)

data HTML = Text String
          | Tag1 String [Attr]
          | Tag String [Attr] [HTML]

eqString      :: String -> String -> Bool 
eqString = undefined

type Requirement = (String, String)

validate :: [Requirement] -> HTML -> Maybe Requirement
validate [(a,b)] html | checkReq 0 0 (a,b) html = Nothing
                      | otherwise = Just (a,b)
validate ((a,b):xs) html | checkReq 0 0 (a,b) html = validate xs html
                         | otherwise = Just (a,b)


checkReq :: Int -> Int -> Requirement -> HTML -> Bool
checkReq diepte laatste (a,b) (Tag tag _ children) | tag' `eqString` a' = rest diepte
                                                   | tag' `eqString` b' && diepte /= laatste + 1 = False
                                                   | otherwise = rest laatste
                                                   where rest x = all (checkReq (diepte+1) x (a,b)) children
                                                         [tag', a', b'] = map map toUpper [tag, a, b]
checkReq diepte laatste (a,b) (Tag1 tag _) | tag `eqString` a = error "Er kunnen geen tags binnen enkele tags voorkomen"
                                           | tag `eqString` b && diepte /= laatste + 1 = False
                                           | otherwise = True
checkReq _ _ _ _ = True

ppHTML :: HTML -> Doc
ppHTML (Text s) = tekst s
ppHTML (Tag1 tag attr) = tekst ("<" ++ tag ++ ppAttr attr ++ ">")
ppHTML (Tag tag attr html) | all simpel html = openTag <|> ppHTMLlijst (<|>) html <|> closeTag
                           | otherwise = openTag <-> springIn 1 (ppHTMLlijst (<->) html) <-> closeTag
                           where openTag  = ppHTML (Tag1 tag attr)
                                 closeTag = tekst ("</" ++ tag ++ ">")

ppHTMLlijst :: (Doc -> Doc -> Doc) -> [HTML] -> Doc
ppHTMLlijst op html = foldr op leeg (map ppHTML html)

ppAttr :: [Attr] -> String
ppAttr [] = []
ppAttr ((a,v):as) = concat [" ", a, "=\"", v, "\""] ++ ppAttr as

simpel :: HTML -> Bool
simpel (Tag _ _ [html]) = simpel html
simpel (Tag _ _ _) = False
simpel _ = True

color :: Int -> Int -> Int -> String
color x y z = '#' : concatMap intToHex [x, y, z]

intToHex :: Int -> String
intToHex i = [hexToChar (i / 16), hexToChar (i `mod` 16)]
             where hexToChar x | x > 9     = chr (x + 55)
                               | otherwise = chr (x + 48)

colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable ctab = Tag "TABLE" [("WIDTH", "400")
                              ,("HEIGHT", "400")
                              ]
                              (map (Tag "TR" []) (map (map colorTD) ctab))

colorTD :: (Int, Int, Int) -> HTML
colorTD (x, y, z) = Tag "TD" [("BGCOLOR", color x y z)] []

ul :: [[HTML]] -> HTML
ul html = Tag "UL" [] (map (Tag "LI" []) html)

h :: Int -> String -> HTML
h i txt = Tag ['H', chr (i + 48)] [] [Text txt]

font :: [Attr] -> [HTML] -> HTML
font = Tag "FONT"

text :: String -> HTML
text = Text

document :: String -> [HTML] -> HTML
document title body = Tag "HTML" [] [Tag "HEAD" [] [Tag "TITLE" [] [Text title]]
                                    ,Tag "BODY" [] body]

kleurenTabel :: HTML
kleurenTabel = document title [h 1 title
                              ,ul [[font [("COLOR", color 255 0 0)] [text "Rood"]
                                         ,text looptvan
                                         ]
                                  ,[font [("COLOR", color 0 255 0)] [text "Groen"]
                                         ,text looptvan
                                         ]
                                  ,[font [("COLOR", color 0 0 255)] [text "Blauw"]
                                         ,text isoveral
                                         ]
                                  ]
                              ,colorTable (groepeer 11 [(x, y, 0) | x <- [0,25..250], y <- [0,25..250] ])
                              ]
               where title = "Kleurentabel"
                     looptvan = " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                     isoveral = " is overal 0"

groepeer :: Int -> [a] -> [[a]]
groepeer _ [] = []
groepeer i x = take i x : groepeer i (drop i x)

-- missing () arond: map toUpper
-- 29,75-101
