module HTML(module Pretty, module HTML) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
showInt       :: Int -> String
showInt = undefined
chr :: Int -> Char
chr = undefined
data HTML
     = SingleTag String [Attributen]
     | ClosedTag String [Attributen] [HTML]
     | Tekst String
type Attributen = String
type Requirement = (String, String)
getTagName :: HTML -> String
getTagName (ClosedTag name _ _) = name
getTagName (SingleTag name _) = name
getTagName (Tekst _) = ""

validate1 :: Requirement -> HTML -> Bool
validate1 (a,b) (ClosedTag naam att (x:xs)) |(eqString (getTagName x) b) = (eqString a naam) && (validate1 (a,b) x) && (validate1 (a,b) (ClosedTag naam att xs))
                                            |otherwise = (validate1 (a,b) x) && (validate1 (a,b) (ClosedTag naam att xs))
validate1 _ _ = True

validate :: [Requirement] -> HTML -> Maybe Requirement
validate (tuple:xs) naam | not(eqString (snd tuple) (getTagName naam)) = if (validate1 tuple naam) then
                                                                               validate xs naam
                                                                         else
                                                                               Just tuple
                         | otherwise = Just tuple
validate [] _ = Nothing
















simple :: HTML -> Bool
simple (Tekst _) = True
simple (SingleTag _ _) = True
simple (ClosedTag _ _ html) = map simple2 html
simple (ClosedTag _ _ []) = True
simple (ClosedTag _ _ _) = False

simple2 :: HTML -> Bool
simple2 (Tekst _) = True
simple2 (SingleTag _ _) = True
simple2 (ClosedTag _ _ [html]) = simple2 html
simple2 (ClosedTag _ _ []) = True
simple2 (ClosedTag _ _ _) = False

prepAttr :: [Attributen] -> Doc
prepAttr [] = tekst ""
prepAttr attributen = tekst " " <|> tekst (unwords attributen)

ppHTML :: HTML -> Doc
ppHTML (ClosedTag name att html) | not(simple (ClosedTag name att html)) = tekst "<" <|> tekst name <|> prepAttr att <|> tekst ">"
                                                                 <-> springIn 1 (ppLijstHTML html)
                                                                 <-> tekst "</" <|> tekst name <|> tekst ">"
                                 | otherwise = tekst "<" <|> tekst name <|> prepAttr att <|> tekst ">"
                                               <|> (ppLijstSimpleHTML html) <|> tekst "</" <|> tekst name <|> tekst ">"
ppHTML (SingleTag name att) = tekst "<" <|> tekst name <|> prepAttr att <|> tekst ">"
ppHTML (Tekst woorden) = tekst woorden

ppLijstHTML :: [HTML] -> Doc
ppLijstHTML [x] = ppHTML x
ppLijstHTML (x:xs) = ppHTML x <->  ppLijstHTML xs
ppLijstHTML [] = tekst ""

ppLijstSimpleHTML :: [HTML] -> Doc
ppLijstSimpleHTML (x:xs) = ppHTML x <|>  ppLijstSimpleHTML xs
ppLijstSimpleHTML [] = tekst ""






color :: Int -> Int -> Int -> String
color a b c = "#" ++ berekenNr a ++ berekenNr b ++ berekenNr c

berekenNr :: Int -> String
berekenNr getal = colorNummer (berekenNrA getal) ++ colorNummer (berekenNrB getal)

berekenNrA :: Int -> Int
berekenNrA getal = getal/16

berekenNrB :: Int -> Int
berekenNrB getal = mod getal 16

colorNummer :: Int -> String
colorNummer nummer | (nummer >=0 && nummer <= 9) = showInt nummer
                   | (nummer >=10 && nummer <= 15) = [chr (nummer + 55)]


colorTable :: [[(Int,Int,Int)]] -> HTML
colorTable kleuren = ClosedTag "TABLE" ["HEIGHT=\"400\"","WIDTH=\"400\""] (maakRijen kleuren)

maakRijen :: [[(Int,Int,Int)]] -> [HTML]
maakRijen (eerste:restRijen) = ClosedTag "TR" [] (maakKol eerste) : maakRijen restRijen
maakRijen [] = []

maakKol :: [(Int,Int,Int)] -> [HTML]
maakKol ((a,b,c):restKleuren) = ClosedTag "TD" ["BGCOLOR=\"" ++ color a b c ++ "\""] [] : maakKol restKleuren
maakKol [] = []


ul :: [[HTML]] -> HTML
ul elem = ClosedTag "UL" [] (maakLi elem)

maakLi :: [[HTML]] -> [HTML]
maakLi (x:xs) = ClosedTag "LI" [] x : maakLi xs
maakLi [] = []


h :: Int -> String -> HTML
h grootte woorden = ClosedTag ("H" ++ (showInt grootte)) [] [Tekst woorden]


font :: [Attributen] -> [HTML] -> HTML
font attr html = ClosedTag "FONT" attr html


text :: String -> HTML
text woorden = Tekst woorden


document :: String -> [HTML] -> HTML
document titel html = ClosedTag "HTML" [] [ ClosedTag "HEAD" [] [ClosedTag "TITLE" [] [Tekst titel]], ClosedTag "BODY" [] html ]


kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel" [h 1 "Kleurentabel",
                                        ul    [ [font ["COLOR=\"" ++ color 255 0 0 ++ "\""] [text "Rood"],
                                                text " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"],

                                                [font ["COLOR=\"" ++ color 0 255 0 ++ "\""] [text "Groen"],
                                                text " loopt van links naar rechts van 0 tot en met 250 in stappen van 25"],

                                                [font ["COLOR=\"" ++ color 0 0 255 ++ "\""] [text "Blauw"],
                                                text " is overal 0"]
                                              ],
                                        colorTable kleurenTuples
                                        ]

kleurenTuples :: [[(Int, Int, Int)]]
kleurenTuples = [[(0, 0, 0), (0, 25, 0), (0, 50, 0), (0, 75, 0), (0, 100, 0), (0, 125, 0), (0, 150, 0), (0, 175, 0), (0, 200, 0), (0, 225, 0), (0, 250, 0)],
           [(25, 0, 0), (25, 25, 0), (25, 50, 0), (25, 75, 0), (25, 100, 0), (25, 125, 0), (25, 150, 0), (25, 175, 0), (25, 200, 0), (25, 225, 0), (25, 250, 0)],
           [(50, 0, 0), (50, 25, 0), (50, 50, 0), (50, 75, 0), (50, 100, 0), (50, 125, 0), (50, 150, 0), (50, 175, 0), (50, 200, 0), (50, 225, 0), (50, 250, 0)],
           [(75, 0, 0), (75, 25, 0), (75, 50, 0), (75, 75, 0), (75, 100, 0), (75, 125, 0), (75, 150, 0), (75, 175, 0), (75, 200, 0), (75, 225, 0), (75, 250, 0)],
           [(100, 0, 0), (100, 25, 0), (100, 50, 0), (100, 75, 0), (100, 100, 0), (100, 125, 0), (100, 150, 0), (100, 175, 0), (100, 200, 0), (100, 225, 0), (100, 250, 0)],
           [(125, 0, 0), (125, 25, 0), (125, 50, 0), (125, 75, 0), (125, 100, 0), (125, 125, 0), (125, 150, 0), (125, 175, 0), (125, 200, 0), (125, 225, 0), (125, 250, 0)],
           [(150, 0, 0), (150, 25, 0), (150, 50, 0), (150, 75, 0), (150, 100, 0), (150, 125, 0), (150, 150, 0), (150, 175, 0), (150, 200, 0), (150, 225, 0), (150, 250, 0)],
           [(175, 0, 0), (175, 25, 0), (175, 50, 0), (175, 75, 0), (175, 100, 0), (175, 125, 0), (175, 150, 0), (175, 175, 0), (175, 200, 0), (175, 225, 0), (175, 250, 0)],
           [(200, 0, 0), (200, 25, 0), (200, 50, 0), (200, 75, 0), (200, 100, 0), (200, 125, 0), (200, 150, 0), (200, 175, 0), (200, 200, 0), (200, 225, 0), (200, 250, 0)],
           [(225, 0, 0), (225, 25, 0), (225, 50, 0), (225, 75, 0), (225, 100, 0), (225, 125, 0), (225, 150, 0), (225, 175, 0), (225, 200, 0), (225, 225, 0), (225, 250, 0)],
           [(250, 0, 0), (250, 25, 0), (250, 50, 0), (250, 75, 0), (250, 100, 0), (250, 125, 0), (250, 150, 0), (250, 175, 0), (250, 200, 0), (250, 225, 0), (250, 250, 0)]
          ]

-- Missing and (...) around map simple2 html
-- 52,31-46
