module Html (module Html, module Pretty) where

import Pretty
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined
eqTuple2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
eqTuple2 = undefined
data HTML = Text String
          | EnkelTag Naam [(Atribuut,Waarde)]
          | DubbelTag Naam [(Atribuut,Waarde)] [HTML]

type Naam = String
type Atribuut = String
type Waarde = String




type Requirement = (String, String)

zoekNaam :: Naam -> HTML -> Bool
zoekNaam _ (Text _) = False
zoekNaam naam (EnkelTag naamTag _) | (eqString naam naamTag) = True
                                   | otherwise = False
zoekNaam naam (DubbelTag naamTag _ htmls) | (eqString naam naamTag) = True
                                          | otherwise = or ( map (zoekNaam naam) htmls)

htmlNaam :: Naam -> HTML -> [HTML]
htmlNaam _ (Text _) = []
htmlNaam naam (EnkelTag naamTag attribs) |(eqString naam naamTag) =  [(EnkelTag naamTag attribs)]
                                         | otherwise = []
htmlNaam naam (DubbelTag naamTag _ htmls) | (eqString naam naamTag) = htmls
                                          | otherwise = concatMap (htmlNaam naam) htmls

validateEnkel :: Requirement -> HTML -> Maybe Requirement
validateEnkel _ (Text _) = Nothing
validateEnkel _ (EnkelTag _ _) = Nothing
validateEnkel (eerste,tweede) (DubbelTag naam attribs htmls) | (or(map (zoekNaam eerste)(htmlNaam tweede (DubbelTag naam attribs htmls)))) = Just (eerste,tweede)
                                                             | otherwise = Nothing

validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (x:xs) html | (eqMaybe (eqTuple2 eqString eqString) (validateEnkel x html) (Just x)) = Just x
                     | otherwise = validate xs html



ppHTML :: HTML -> Doc
ppHTML (Text string) = tekst string
ppHTML (EnkelTag naam attribs) = tekst ("<" ++ naam ++ (listStringAtrVal attribs) ++ ">")
ppHTML (DubbelTag naam attribs htmls) | ((length htmls) == 0) = tekst ("<" ++ naam ++ (listStringAtrVal attribs) ++ ">" ++ "</" ++ naam ++ ">")
                                      | (isSimple (DubbelTag naam attribs htmls)) = tekst ("<" ++ naam ++ (listStringAtrVal attribs) ++ ">" ++ (ppHTMLSimpList htmls) ++ "</" ++ naam ++ ">")
                                      | otherwise = (tekst ("<" ++ naam ++ (listStringAtrVal attribs) ++ ">")) <-> (springIn 1 (ppHTMLList htmls)) <-> (tekst ("</" ++ naam ++ ">"))



ppHTMLList :: [HTML] -> Doc
ppHTMLList (x:xs) | (length xs >= 1) = (ppHTML x) <-> (ppHTMLList xs)
                  | otherwise = (ppHTML x)

listStringAtrVal :: [(String,String)] -> String
listStringAtrVal [] = ""
listStringAtrVal ((attrib,waarde):xs) = " " ++ attrib ++ "=" ++ waarde ++ (listStringAtrVal xs)

isSimple :: HTML -> Bool
isSimple (Text _) = True
isSimple (EnkelTag _ _) = True
isSimple (DubbelTag _ _ htmls) | ((length htmls) /= 1) = False
                               | and (map isSimple htmls) = True
                               | otherwise = False

ppHTMLSimpList :: [HTML] -> String
ppHTMLSimpList [] = ""
ppHTMLSimpList (x:xs) = (ppHTMLSimple x) ++ (ppHTMLSimpList xs)

ppHTMLSimple :: HTML -> String
ppHTMLSimple (Text string) = string
ppHTMLSimple (EnkelTag naam []) = ("<" ++ naam ++ (listStringAtrVal []) ++ ">")
ppHTMLSimple (EnkelTag naam [(attrib,waarde)]) = ("<" ++ naam ++ (listStringAtrVal [(attrib,waarde)]) ++ ">")
ppHTMLSimple (DubbelTag naam [] html) = ("<" ++ naam ++ ">" ++ (ppHTMLSimple (head html)) ++ "</" ++ naam ++ ">")
ppHTMLSimple (DubbelTag naam [(attrib,waarde)] html) = ("<" ++ naam ++ (listStringAtrVal [(attrib,waarde)]) ++  ">" ++ (ppHTMLSimple (head html)) ++ "</" ++ naam ++ ">")





colorEnkel :: Int -> String
colorEnkel r = hexConvert(r/16) ++ hexConvert(rem r 16)

hexConvert :: Int -> String
hexConvert getal | ((getal >= 0) && (getal <= 9)) = [chr (getal + ord('0'))]
                 | (getal > 9) = [chr ((getal -10) +ord('A'))]

color :: Int -> Int -> Int -> String
color r g b = "\"#" ++ (colorEnkel r) ++ (colorEnkel g) ++ (colorEnkel b)  ++ "\""



colorElement :: (Int,Int,Int) -> HTML
colorElement (r,g,b) = (DubbelTag "TD" [("BGCOLOR",(color r g b))] [])

colorTableKolom :: [HTML] -> HTML
colorTableKolom elementKolom = DubbelTag "TR" [] elementKolom

convertKleurHtmlKolom :: [(Int,Int,Int)] -> [HTML]
convertKleurHtmlKolom [] = []
convertKleurHtmlKolom (x:xs) = (colorElement x) : convertKleurHtmlKolom (xs)

colorTabKolom :: [(Int,Int,Int)] -> HTML
colorTabKolom list = colorTableKolom (convertKleurHtmlKolom list)

convertKleurHtmlMatrix :: [[(Int,Int,Int)]] -> [HTML]
convertKleurHtmlMatrix [] = []
convertKleurHtmlMatrix (x:xs) = (colorTabKolom x) : convertKleurHtmlMatrix xs

colorTable :: [[(Int,Int,Int)]] -> HTML
colorTable list = DubbelTag "TABLE"[("WIDTH","\"400\""),("HEIGHT","\"400\"")] (convertKleurHtmlMatrix list)



liLijst :: [[HTML]] -> [HTML]
liLijst [] = []
liLijst (x:xs) = (DubbelTag "LI" [] x) : liLijst xs

ul :: [[HTML]] -> HTML
ul htmlList = DubbelTag "UL" [] (liLijst htmlList)



h :: Int -> HTML
h getal = DubbelTag ('H' : chr (getal + ord('0')) : []) [] []





font :: [(Atribuut,Waarde)] -> [HTML] -> HTML
font attribs htmllist = DubbelTag "FONT" attribs htmllist



text :: String -> HTML
text string = Text string



document :: String -> [HTML] -> HTML
document titel body = DubbelTag "HTML" [] [(DubbelTag "HEAD" [] [DubbelTag "TITLE" [] [Text titel]]),(DubbelTag "BODY" [] body)]



bovenTekst :: HTML
bovenTekst = ul [rood,rood,rood]
             where rood = [(font [("COLOR", (color 256 0 0))] [Text "Rood"]),[Text " loopt van boven naar onder van 0 tot en met 250 in stappen van 25"]]




kleurTabRijRood :: (Int,Int,Int) -> [(Int,Int,Int)]
kleurTabRijRood (r,g,b) | (r <= 256) = (r,g,b) : kleurTabRijRood ((r+25),g,b)
                        | otherwise = []

kleurTabKolomGroen :: (Int,Int,Int) -> [(Int,Int,Int)]
kleurTabKolomGroen (r,g,b) | (g <= 256) = (r,g,b) : kleurTabKolomGroen (r,(g + 25),b)
                           | otherwise = []

kleurTableMatrix :: (Int,Int,Int) -> [[(Int,Int,Int)]]
kleurTableMatrix (r,g,b) = map kleurTabKolomGroen (kleurTabRijRood (r,g,b))

kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel" [(h 1),bovenTekst,(colorTable (kleurTableMatrix (0,0,0)))]

main :: IO ()
main = layout (ppHTML kleurenTabel)

-- [...] should be (...)
-- 160,78-152
