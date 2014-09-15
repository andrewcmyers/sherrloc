module HTML(module Pretty, module HTML) where

import Pretty

data HTML = Tekst String
          | Enkel Tag HTML
          | Dubbel Tag [HTML]
eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
type Tag = (Naam,[Attribute])
type Naam = String
type Attribute = (Arg,Waarde)
type Arg = String
type Waarde = String
type Requirement = (String, String)
validate :: [Requirement] -> HTML -> Maybe Requirement

validate [] _ = Nothing
validate (req:reqs) html | check req html = validate reqs html
                         | otherwise = Just req

check :: Requirement -> HTML -> Bool
check _ (Tekst _) = True
check req (Enkel _ html) = check req html
check req (Dubbel tag htmls) | eqString (fst req) (fst tag) = True
                             | eqString (snd req) (fst tag) = False
                             | otherwise = and (map (check req) htmls)

goed :: HTML
goed = Enkel ("BR",[]) (Dubbel ("UL",[]) [(Dubbel ("IL",[]) [(Tekst "bla")])])
fout :: HTML
fout = Enkel ("BR",[]) (Dubbel ("IL",[]) [(Dubbel ("UL",[]) [(Tekst "bla")])])

color :: Int -> Int -> Int -> String
color r g b = "#" ++ intToHex r ++ intToHex g ++ intToHex b

intToHex :: Int -> [Char]
intToHex getal = [hex (getal/16)] ++ [hex (rem getal 16)]

hex :: Int -> Char
hex getal |getal >= 0 && getal <= 9 = chr (getal + 48)
          |getal > 9 = chr (getal + 55)

text :: String -> HTML
text string = Tekst string

h :: Int -> String -> HTML
h x string = Dubbel ((maakNaam x),[]) [Tekst string]

maakNaam :: Int -> [Char]
maakNaam x = ['H']++[chr (x + 48)]

ul :: [[HTML]] -> HTML
ul html = Dubbel ("UL",[]) (maakLI html)

maakLI :: [[HTML]]->[HTML]
maakLI [] = []
maakLI (html:htmls) = (Dubbel ("LI",[]) (maakAanElkaar html)): maakLI htmls

maakAanElkaar :: [HTML]->[HTML]
maakAanElkaar [] = []
maakAanElkaar ((Tekst a):(Tekst b):htmls) = maakAanElkaar ((Tekst (a++b)): htmls)
maakAanElkaar (html:htmls) = html:(maakAanElkaar htmls)


font :: [Attribute] -> [HTML] -> HTML
font attributes html = Dubbel ("FONT",attributes) html

document :: String -> [HTML] -> HTML
document titel html = Dubbel ("HTML",[]) [(Dubbel ("HEAD",[]) [(Dubbel ("TITLE",[]) [(Tekst titel)])]),(Dubbel ("BODY",[]) html)]





kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel" [(h 1 "Kleurentabel"),(ul [fontRood,fontGroen,fontBlauw]),(colorTable (maakColorRow 0))]
                                 where fontRood = [(font [("COLOR","#FF0000")] [(Tekst "Rood ")]),(Tekst "loopt van boven naar onder van 0 tot en met 250 in stappen van 25")];
                                        fontGroen = [(font [("COLOR","#00FF00")] [(Tekst "Groen ")]),(Tekst "loopt van links naar rechts van 0 tot en met 250 in stappen van 25")];
                                         fontBlauw = [(font [("COLOR","#0000FF")] [(Tekst "Blauw ")]),(Tekst "is overal 0")]


maakColorRow :: Int -> [[(Int,Int,Int)]]
maakColorRow 250 = [(maakColorColumn 250 0)]
maakColorRow r = (maakColorColumn r 0):(maakColorRow (r+25))

maakColorColumn :: Int->Int->[(Int,Int,Int)]
maakColorColumn r 250 = [(r,250,0)]
maakColorColumn r g = (r,g,0):maakColorColumn r (g+25)





maakRijen :: Int->[HTML]

maakRijen 250 = [(Dubbel ("TR",[]) (maakKolommen 250 0))]
maakRijen r = (Dubbel ("TR",[]) (maakKolommen r 0)):maakRijen (r+25)

maakKolommen :: Int->Int->[HTML]

maakKolommen r 250 = [(Dubbel ("TD",[("BGCOLOR",(color r 250 0))]) [(Tekst "")])]















colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable kleuren = (Dubbel ("TABLE",[("WIDTH","400"),("HEIGHT","400")]) (maakTableRow kleuren))

maakTableRow :: [[(Int,Int,Int)]]->[HTML]
maakTableRow [] = []
maakTableRow (row:rows) = (Dubbel ("TR",[]) (maakTableColumn row)):(maakTableRow rows)

maakTableColumn :: [(Int,Int,Int)]->[HTML]
maakTableColumn [] = []
maakTableColumn ((r,b,g):columns) = (Dubbel ("TD",[("BGCOLOR",(color r b g))]) [(Tekst "")]):(maakTableColumn columns)
























simpel :: [HTML]->Bool
simpel [] = True
simpel (html:htmls) | isTekst html = simpel htmls
                    | isEnkelTag html = simpel htmls
                    | isLengteEen html = simpel htmls
                    | otherwise = False

isTekst :: HTML->Bool
isTekst (Tekst _) = True
isTekst _ = False

isEnkelTag :: HTML->Bool
isEnkelTag (Enkel _ _) = True
isEnkelTag _ = False

isDubbelTag :: HTML->Bool
isDubbelTag (Dubbel _ _) = True
isDubbelTag _ = False
isLengteEen :: HTML->Bool
isLengteEen (Dubbel _ [html]) | isTekst html = True
                              | isEnkelTag html = True
                              | isLengteEen html = True
                              | otherwise = False
isLengteEen _ = False

foutesimpel :: Bool
foutesimpel = simpel [(Dubbel ("IL",[]) [(Dubbel ("B",[]) [(Tekst "1")]),(Tekst "12")]),(Dubbel ("IL",[]) [(Tekst "3")])]
goedesimpel :: Bool
goedesimpel = simpel [(Dubbel ("B",[]) [(Tekst "tekst")]),(Tekst "je")]


main :: IO ()
main = do {putStrLn (putStr(ppHTML kleurenTabel))}


ppHTML :: HTML -> Doc
ppHTML html = ppBreekOp html

ppBreekOp :: HTML -> Doc
ppBreekOp (Tekst html) = [html]
ppBreekOp (Enkel tag html) = [(schrijfTag tag)] <|> (ppBreekOp html)
ppBreekOp (Dubbel tag htmls) | simpel htmls = [(schrijfTag tag)] <|> (ppBreekSimpelLijstOp htmls) <|> [("</"++(fst tag)++">")]
                             | otherwise = [(schrijfTag tag)] <-> (springIn 2 (ppBreekLijstOp htmls)) <-> [("</"++(fst tag)++">")]

ppBreekSimpelLijstOp :: [HTML] -> Doc
ppBreekSimpelLijstOp [] = []
ppBreekSimpelLijstOp (html:htmls) = ppBreekOp html <|> ppBreekSimpelLijstOp htmls

ppBreekLijstOp :: [HTML] -> Doc
ppBreekLijstOp [] = []
ppBreekLijstOp (html:htmls) = ppBreekOp html <-> ppBreekLijstOp htmls
















schrijfTag :: Tag->String
schrijfTag (tagNaam,attr) = ("<"++tagNaam)++(schrijfAttr attr)++">"

schrijfAttr :: [Attribute]->String
schrijfAttr [] = ""
schrijfAttr ((arg,waarde):attrs) = (" "++arg++"=\""++waarde++"\"")++schrijfAttr attrs

-- putStr should be unlines
-- 186,22-48   186,22-27
