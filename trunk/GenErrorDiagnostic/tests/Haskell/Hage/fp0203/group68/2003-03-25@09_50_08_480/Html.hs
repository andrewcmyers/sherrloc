module Html(module Pretty, module Html) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
showInt       :: Int -> String
showInt = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined

data HTML = Tekst String
          | ET String [Attribuut]
          | DT String [Attribuut] [HTML]

type Attribuut = (AttrNm, Waarde)
type AttrNm = String
type Waarde = String
type Requirement = (String, String)


validate :: [Requirement] -> HTML -> Maybe Requirement
validate lijst html = foldl combine Nothing (map (validateSingle html) lijst)
                            where combine rest req = case rest of
                                          (Just x) -> (Just x)
                                          Nothing  -> case req of
                                                    Nothing -> Nothing
                                                    (Just x)-> (Just x)


validateSingle ::  HTML -> Requirement -> Maybe Requirement
validateSingle html requirement = testEigenschap requirement "" html


testEigenschap :: Requirement -> String -> HTML -> Maybe Requirement
testEigenschap _ _ (Tekst _) = Nothing
testEigenschap (ouderV, kindV) ouder (DT kind _ html)
                 | (kind `eqString` kindV) && not(ouder `eqString` ouderV) = Just (ouderV, kindV)
                 | otherwise = testELijst (map (testEigenschap (ouderV, kindV) kind) html)
testEigenschap (ouderV, kindV) ouder (ET kind _)
                 | (kind `eqString` kindV) && not(ouder `eqString` ouderV) = Just (ouderV, kindV)
                 | otherwise = Nothing
testEigenschap _ _ _ = Nothing


testELijst :: [Maybe Requirement] -> Maybe Requirement
testELijst [] = Nothing
testELijst (x:xs) | eqMaybe eqRequirement x Nothing = testELijst xs
                   | otherwise = x


eqRequirement :: Requirement -> Requirement -> Bool
eqRequirement (ouder1,kind1) (ouder2, kind2) = eqString ouder1 ouder2 && eqString kind1 kind2




simpel :: HTML -> Bool
simpel (Tekst _) = True
simpel (ET _ _ )= True
simpel (DT _ _ html )=  (and (map simpel2 html))
simpel (DT _ _ _) = False


simpel2 :: HTML -> Bool
simpel2 (DT _ _ [_]) = True
simpel2 (DT _ _ _ ) = False
simpel2 (Tekst _) = True
simpel2 (ET _ _ ) = True


ppHTML     :: HTML -> Doc
ppHTML html | simpel html = ppSimpel html
            | otherwise = ppIngewikkeld html


printTagOpen :: String -> [Attribuut] -> Doc
printTagOpen naam attributen = tekst ("<" ++ naam ++ " " ++ (concatMap printAttribuut attributen) ++ ">")


printTagSluit :: String -> Doc
printTagSluit naam = tekst ("<\\" ++ naam ++ ">")


printAttribuut :: Attribuut -> String
printAttribuut (naam,waarde) = naam ++ "=\"" ++ waarde ++ "\" "


ppSimpel :: HTML -> Doc
ppSimpel (Tekst verhaal) = tekst verhaal
ppSimpel (ET naam attributen) = printTagOpen naam attributen
ppSimpel (DT naam attributen html) = printTagOpen naam attributen
                                     <|> (foldr (<|>) leeg (map ppSimpel html))
                                     <|> printTagSluit naam


ppIngewikkeld :: HTML -> Doc
ppIngewikkeld (DT naam attributen html) = (printTagOpen naam attributen) <->
                                         springIn 1 (concatMap ppHTML html) <->
                                         (printTagSluit naam)


color :: Int -> Int -> Int -> String
color g1 g2 g3 = "#" ++ (map vervang  [(div g1 16), (mod g1 16),(div g2 16), (mod g2 16),(div g3 16), (mod g3 16)])


vervang :: Int -> Char
vervang getal |(getal == 10) = 'A'
              |(getal == 11) = 'B'
              |(getal == 12) = 'C'
              |(getal == 13) = 'D'
              |(getal == 14) = 'E'
              |(getal == 15) = 'F'
              |otherwise = head (showInt getal)




colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable kleurenMatrix = (DT "TABLE" [("WIDTH","400"),("HEIGHT","400")]
                            (map maakKleurRegel (map (map maakKleurCel) kleurenMatrix))
                           )


maakKleurCel ::  (Int, Int, Int) -> HTML
maakKleurCel (g1, g2, g3) = DT "TD" [("BGCOLOR",color g1 g2 g3)] []


maakKleurRegel :: [HTML] -> HTML
maakKleurRegel kleurcellen = DT "TR" [] kleurcellen


ul :: [[HTML]] -> HTML
ul elementen = DT "UL" [] (map maakLI elementen)


maakLI :: [HTML] -> HTML
maakLI element = DT "LI" [] element


h          :: Int -> String -> HTML
h kopnr verhaal = DT ("H"++ (showInt kopnr)) [] [Tekst verhaal]


font       :: [Attribuut] -> [HTML] -> HTML
font attributen html = DT "FONT" attributen html


text       :: String -> HTML
text verhaal = Tekst verhaal


document   :: String -> [HTML] -> HTML
document titel inhoud = DT "HTML" [] [DT "HEAD" [] [DT "TITLE" [] [Tekst titel]], DT "BODY" [] [inhoud]]






kleurenM :: [[(Int, Int, Int)]]
kleurenM= [[(33, 33, 33),(33, 33, 33),(33, 33, 33)],[(33, 33, 33),(33, 33, 33),(33, 33, 33)],[(33, 33, 33),(33, 33, 33),(33, 33, 33)]]


reqLijst :: [Requirement]
reqLijst = [("LU","LI"), ("HTML", "BODY"), ("HTML","HEAD"), ("BODY", "TABLE"), ("BODY", "H"), ("BODY", "HR")]

test :: HTML
test = Tekst "Hallo"

test1 :: HTML
test1 = ET "HR" []

test2 :: HTML
test2 = DT "html" []    [DT "head" [] [Tekst "Hallo"],
                         DT "body" [] [Tekst "Hallo2"]]
test3 :: HTML
test3 = DT "html" [("bgcolor","red")]
                                [DT "head" [] [Tekst "Hallo"],
                                 DT "body" [] [Tekst "Hallo2"]]

test4 :: HTML
test4 = DT "html" [("bgcolor","red")]
                                [DT "head" [] [Tekst "Hallo"],
                                 DT "body" [] [Tekst "Hallo2", ET "br" []]]

test5 :: HTML
test5 = DT "HTML" [("bgcolor","red")]
                                [DT "HEAD" [] [Tekst "Hallo", DT "TABLE" [] [ET "HR" []]],
                                 DT "BODY" [] [Tekst "Hallo2", ET "HR" []]

                                 ]
test6:: HTML
test6 = DT "body" [] [ET "hr" [], ET "br" [], DT "table" [] [DT "tr" [] []]]

test7 :: HTML
test7 = DT "HTML" [("bgcolor","red")]
                                [
                                 DT "BODY" [] [DT "table" [] [DT "tr" [] [DT "td" [] [Tekst "hallo", ET "hr" []]]]]

                                 ]


test8 :: HTML
test8 = DT "UL" [] [DT "LI" [] [DT "B" [] [Tekst "1"], Tekst "12"], DT "LI" [] [Tekst "3"]]

-- [inhoud] should be: inhoud
-- 153,97-102   153,96-103
