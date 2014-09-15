module Htmlprac where

import Pretty


data HTML
 = Tekst String
 | EnkeleTag String [Attribute]
 | DubbelTag String [Attribute] [HTML]

type Attribute = (String, String)

eqString      :: String -> String -> Bool 
eqString = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined
showInt       :: Int -> String
showInt = undefined









type Requirement = (String, String)


validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _                    = Nothing
validate (kopReq:staartReq) html | eqMaybe eqReq (controleer kopReq "" html) Nothing = validate staartReq html
                                 | otherwise                                         = Just kopReq

controleer :: Requirement -> String -> HTML -> Maybe Requirement
controleer _ _ (Tekst _ )                             = Nothing
controleer req buitenTag (EnkeleTag naam _ )          | checkReq req buitenTag naam = Just req
                                                      | otherwise                   = Nothing
controleer req buitenTag (DubbelTag naam _ htmlLijst) | checkReq req buitenTag naam = Just req
                                                      | otherwise                   = controleerLijst req naam htmlLijst

checkReq :: Requirement -> String -> String -> Bool
checkReq (buitenReq, binnenReq) buitenTag naam = eqString binnenReq naam && not (eqString buitenReq buitenTag)

controleerLijst :: Requirement -> String -> [HTML] -> Maybe Requirement
controleerLijst _ _ []                       = Nothing
controleerLijst req buitenTag (kopHtml:staartHtml) | eqMaybe eqReq doeKop Nothing = controleerLijst req buitenTag staartHtml
                                             | otherwise                    = doeKop
                                                        where doeKop = controleer req buitenTag kopHtml

eqReq:: Requirement -> Requirement -> Bool
eqReq (links1, rechts1) (links2, rechts2) = eqString links1 links2 && eqString rechts1 rechts2


ppHTML1 :: HTML -> IO()
ppHTML1 html = layout (ppHTML html)


ppHTML :: HTML -> Doc
ppHTML (Tekst woorden)                        = tekst (woorden)
ppHTML (EnkeleTag naam attLijst)           = openTag naam attLijst
ppHTML (DubbelTag naam attLijst htmlLijst) | simpelLijst htmlLijst
                                              = openTag naam attLijst <|> printHtml htmlLijst <|> closeTag naam
                                           | otherwise
                                              = openTag naam attLijst
                                                <-> springIn 1 (lastigLijst htmlLijst)
                                                <-> closeTag naam


simpelLijst :: [HTML] -> Bool
simpelLijst []                   = True
simpelLijst (kopHtml:staartHtml) = simpel kopHtml && simpelLijst staartHtml

simpel :: HTML -> Bool
simpel (Tekst _ )                = True
simpel (EnkeleTag _ _ )          = True
simpel (DubbelTag _ _ htmlLijst) | length htmlLijst == 1 = simpel (head htmlLijst)
                                 | otherwise             = False


lastigLijst :: [HTML] -> Doc
lastigLijst []                   = leeg
lastigLijst (kopHtml:staartHtml) = ppHTML kopHtml <-> lastigLijst staartHtml

openTag :: String -> [Attribute] -> Doc
openTag naam attLijst = tekst "<" <|> tekst naam <|> printAtts attLijst <|> tekst ">"

closeTag :: String -> Doc
closeTag naam = tekst "</" <|> tekst naam <|> tekst ">"

printHtml :: [HTML] -> Doc
printHtml []                   = leeg
printHtml (kopHtml:staartHtml) = ppHTML kopHtml <|> printHtml staartHtml

printAtts :: [Attribute] -> Doc
printAtts []                               = leeg
printAtts ((attNaam, attWaarde):staartAtt) = tekst " " <|> "/"" <|> tekst attNaam <|> "/"" <|> tekst "=" <|> tekst attWaarde <|> printAtts staartAtt


color :: Int -> Int -> Int -> String
color r g b = "#" ++ hexa (maakRGB r) ++ hexa (maakRGB g) ++ hexa (maakRGB b)

maakRGB :: Int -> Int
maakRGB getal | getal > 255 = 255
              | getal < 0   = 0
              | otherwise   = getal

hexa :: Int -> String
hexa rgbWaarde = zoekOp hexTabel (rgbWaarde / 16) ++ zoekOp hexTabel (rem rgbWaarde 16)

zoekOp :: [(Int, String)] -> Int -> String
zoekOp ((numGetal,hexString):staartTabel) getal | numGetal == getal = hexString
                                                | otherwise         = zoekOp staartTabel getal

hexTabel :: [(Int, String)]
hexTabel =
 [ (0,"0"), (1,"1"), (2,"2"), (3,"3"), (4,"4"), (5,"5"), (6,"6"), (7,"7"), (8,"8"), (9,"9")
 , (10,"A"), (11,"B"), (12,"C"), (13,"D"), (14,"E"), (15,"F")
 ]


colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable kleuren = DubbelTag "TABLE" [("WIDTH","400"),("HEIGHT","400")] (kleurTabel kleuren)

kleurTabel :: [[(Int, Int, Int)]] -> [HTML]
kleurTabel []                   = []
kleurTabel (kopRij:staartTabel) = DubbelTag "TR" [] (kleurRij kopRij) : kleurTabel staartTabel

kleurRij :: [(Int, Int, Int)] -> [HTML]
kleurRij []                 = []
kleurRij (kopCel:staartRij) = (kleurCel kopCel : kleurRij staartRij)

kleurCel :: (Int, Int, Int) -> HTML
kleurCel (r,g,b) = DubbelTag "TD" [("BGCOLOR",color r g b)] []


ul :: [[HTML]] -> HTML
ul html = DubbelTag "UL" [] (li html)

li :: [[HTML]] -> [HTML]
li []                   = []
li (kopHtml:staartHtml) = DubbelTag "LI" [] kopHtml : li staartHtml


h :: Int -> String -> HTML
h niveau kopje = DubbelTag ("H" ++ (showInt niveau)) [] [Tekst kopje]


font :: [Attribute] -> [HTML] -> HTML
font attLijst htmlLijst = DubbelTag "FONT" attLijst htmlLijst


text :: String -> HTML
text woorden = Tekst woorden


document :: String -> [HTML] -> HTML
document titel inhoud = DubbelTag "HTML" [] [DubbelTag "HEAD" [] [DubbelTag "TITLE" [] [Tekst titel] ], DubbelTag "BODY" [] inhoud ]


kleurenTabel :: HTML
kleurenTabel = document "kleurentabel"
                 [ h 1 "kleurentabel"
                 , ul [ [font [("COLOR","#FF0000")] [text "Rood"], text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"]
                      , [font [("COLOR","#00FF00")] [text "Groen"], text "loopt van links naar rechts van 0 tot en met 250 in stappen van 25"]
                      , [font [("COLOR","#0000FF")] [text "Blauw"], text "is overal 0"]
                      ]
                 , colorTable maakKleurenTabel
                 ]

maakKleurenTabel :: [[(Int,Int,Int)]]
maakKleurenTabel = groepeer 11 [ (r,g,0) | r <- [0,25..250], g <- [0,25..250] ]

groepeer :: Int -> [a] -> [[a]]
groepeer _ [] = []
groepeer aantal lijst = take aantal lijst : groepeer aantal (drop aantal lijst)

-- missing tekst before the strings
-- 98,60-62   98,87-90
