module Testing(module Testing, module Pretty) where
import Pretty
data HTML      =   Tekst String
                 | Tag String [Attribuut]
                 | DoubleTag String [Attribuut] [HTML]
type Attribuut = (String, String)
eqString      :: String -> String -> Bool 
eqString = undefined
showInt       :: Int -> String
showInt = undefined
vb :: HTML
vb =  DoubleTag "BODY" [("BGCOLOR","#112233")] [Tag "IMG" [("SRC","hoi.JPG")] ]

vb2 :: HTML
vb2 = DoubleTag "UL" [] [DoubleTag "LI" [] [Tekst "1"], DoubleTag "lk" [] [Tekst "2"]]

type Requirement = (String, String)

valideer :: HTML -> Requirement -> Maybe Requirement
valideer (Tekst _) _ = Nothing
valideer (Tag _ _) _ = Nothing
valideer (DoubleTag tag _ htmls) req  |(eqString tag) (snd req) = Just req
                                     | eqString tag (fst req) = Nothing
                                     | otherwise = (map (\html -> valideer html req) htmls)

checkReq :: [Maybe Requirement] -> Maybe Requirement
checkReq _                   = Nothing
checkReq (Nothing:maybeReqs) = checkReq maybeReqs
checkReq (Just req:_)        = Just req



validate :: HTML -> [Requirement] -> Maybe Requirement
validate html reqs = checkReq (map (valideer html) reqs)



requirements :: [Requirement]
requirements = [("UL", "LI"), ("HTML", "BODY"), ("HTML","HEAD"), ("BODY", "TABLE"), ("BODY", "HR"), ("BODY", "H")]




































color :: Int -> Int -> Int -> String
color no1 no2 no3 | (no1 < 256) && (no2 < 256) && (no3 < 256) = "#" ++ zetHelemaal no1 ++ zetHelemaal no2 ++ zetHelemaal no3

zetHelemaal :: Int -> String
zetHelemaal getal = zetOm (getal/16) ++ zetOm (rem getal 16)

zetOm :: Int -> String
zetOm formule | formule ==10   ="A"
              | formule ==11   ="B"
              | formule ==12   ="C"
              | formule ==13   ="D"
              | formule ==14   ="E"
              | formule ==15   ="F"
              | otherwise      = showInt formule



colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable kleurenlijst = DoubleTag "TABLE" [("WIDTH","400"), ("HEIGHT","400")] (map functie kleurenlijst)

functie ::  [(Int, Int, Int)] -> HTML
functie lijst = DoubleTag "TR" [] (map functie2 lijst)

functie2 :: (Int, Int, Int) -> HTML
functie2 (a, b, c) = DoubleTag "TD" [("BGCOLOR",color a b c)] []



ul :: [[HTML]] -> HTML
ul html = DoubleTag "UL" [] (map (\elem -> DoubleTag "LI" [] elem) html)



h :: Int -> String ->HTML
h grootte string = DoubleTag ("H" ++ (showInt grootte)) [] [Tekst string]



text :: String -> HTML
text string = Tekst string



font :: [Attribuut] -> [HTML] -> HTML
font attributen html = DoubleTag "FONT" attributen html



document :: String -> [HTML] -> HTML
document titel inhoud = DoubleTag "HTML" [] [DoubleTag "HEAD" [] [DoubleTag "TITLE" [] [Tekst titel]], DoubleTag "Body" [] inhoud]



kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel"
                        [h 1 "Kleurentabel",
                         ul [
                              [font [("COLOR","#FF0000")] [Tekst "Rood"],  Tekst "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"],
                              [font [("COLOR","#00FF00")] [Tekst "Groen"], Tekst "loopt van links naar rechts van 0 tot en met 250 in stappen van 25"],
                              [font [("COLOR","#0000FF")] [Tekst "Blauw"], Tekst "is overal 0"]
                            ],
                         colorTable [[(r,g,0) | r<-[0,25..250]] |  g<-[0,25..250]]
                        ]

-- should be: checkReq (map ...)
-- 24,52-91
