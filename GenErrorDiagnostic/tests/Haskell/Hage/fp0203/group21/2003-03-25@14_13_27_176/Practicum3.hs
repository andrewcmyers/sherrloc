module Practicum3(module Practicum3, module Pretty) where
import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
data HTML = Text String
          | TagEnkel Tag [Attribuut]
          | TagTwee Tag [Attribuut] [HTML]
data Attribuut = Attribuut Naam Waarde
type Naam = String
type Waarde = String
type Tag = String
type Requirement = (String, String)
validate   :: [Requirement] -> HTML -> Maybe Requirement
validate [] html     = Nothing
validate (x:xs) html =  if (validater x html)
                        then validate xs html
                        else Just x
        where
                validater :: Requirement -> HTML -> Bool
                validater (totaal, variabele) (TagTwee tag x html)
                                                        | eqString variabele tag                = False
                                                        | eqString variabele totaal             = True
                                                        | otherwise                             = foldr (&&) True (map (validater (totaal, variabele)) html)
                validater (totaal, variabele) _  = True


ppHTML :: HTML -> Doc
ppHTML (Text x) = tekst x
ppHTML (TagEnkel tag []) = tekst "<" <|> tekst tag <|> tekst ">"
ppHTML (TagEnkel tag attr) = tekst "<" <|> tekst tag  <+> (concatMap ppAttr attr) <|> tekst ">"
ppHTML (TagTwee tag attr (x:[]))
                                        | checkSimpel x = tekst "<" <|> tekst tag <+> (concatMap ppAttr attr) <|> tekst ">" <+> ppHTML x <+> tekst "</" <|> tekst tag <|> tekst ">"
                                        | otherwise     = tekst "<" <|> tekst tag <+> (concatMap ppAttr attr) <|> tekst ">" <|> verticaleLijst (ppHTML x) <-> tekst "</" <|> tekst tag <|> tekst ">"
ppHTML (TagTwee tag attr html) = tekst "<" <|> tekst tag <+> (concatMap ppAttr attr) <|> tekst ">" <|> verticaleLijst (map ppHTML html) <-> tekst "</" <|> tekst tag <|> tekst ">"

ppAttr :: Attribuut -> Doc
ppAttr (Attribuut naam waarde) = tekst naam <+> tekst "=" <+> tekst waarde

checkSimpel :: HTML -> Bool
checkSimpel (Text _) = True
checkSimpel (TagEnkel _ _ ) = True
checkSimpel (TagTwee _ _ html)
                                        | (length html) == 1                    = checkSimpel (head html)
                                        | otherwise                                     = False




color :: Int -> Int -> Int -> String
color rood groen blauw = "#" ++ maakHex rood ++ maakHex groen ++ maakHex blauw
        where
                maakHex getal = chr (getalOfChar (getal/16)) : chr (getalOfChar (getal - (16*(getal/16)))) : []

getalOfChar waarde
                | waarde >= 10          = ord 'A' + (waarde -10)
                | otherwise             = ord '0' + waarde

colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable lijst = TagTwee "TABLE" [Attribuut "WIDTH" "400", Attribuut "HEIGHT" "400"] (map maakRij lijst)

maakRij :: [(Int, Int, Int)] -> HTML
maakRij lijst = TagTwee "TR" [] (map maakKolom lijst)

maakKolom :: (Int, Int, Int) -> HTML
maakKolom (x,y,z) = TagTwee "TD" [Attribuut "BGCOLOR" (color x y z)] []


ul :: [[HTML]] -> HTML
ul x = TagTwee "UL" [] (map maakLI x)
        where
                maakLI x = TagTwee "LI" [] x


h :: Int -> String -> HTML
h int string = TagTwee (headerTag int) [] [Text string]
        where
                headerTag int = 'H' : chr ( ord '0' + int) : []

font :: [Attribuut] -> [HTML] -> HTML
font attrs htmls = TagTwee "FONT" attrs htmls

text :: String -> HTML
text string = Text string

document :: String -> [HTML] -> HTML
document titel body = TagTwee "HTML" [] [TagTwee "HEAD" [] [TagTwee "TITLE" [] [Text titel]] , TagTwee "BODY" [] body]

kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel"
                                                [ (h 1 "Kleurentabel")
                                        , ul [ [ font [Attribuut "COLOR" (color 255 0 0)] [text "Rood"]
                                              , text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                                              ]
                                            , [ font [Attribuut "COLOR" (color 0 255 0)] [text "Groen"]
                                              , text "loopt van links naar rechts van 0 tot en met 250 in stappen van 25"
                                              ]
                                            , [ font [Attribuut "COLOR" (color 0 0 255)] [text "Blauw"]
                                              , text "is overal 0"
                                              ]
                                            ]
                                        , colorTable (maakKleurenLijst)
                                        ]



f g b = take 11 [ (r, g, b) |  r <- [0,25..] ]

f2 (r,groen,b) = take 11 [ (r, groen, b) | groen <- [0,25..] ]

maakKleurenLijst = map f2 ( f 0 0)




main :: IO ()
main = layout ( ppHTML kleurenTabel)









voorbeeld :: HTML
voorbeeld = Text "Hallo"


voorbeeld3 = TagEnkel "BR" []


voorbeeld2 :: HTML
voorbeeld2 = TagTwee "HTML" []  [
                                 TagTwee "BODY" [] [],
                                 TagTwee "HEAD" [] [voorbeeld, voorbeeld3],
                                 voorbeeld,
                                 voorbeeld3
                                ]

-- should delete: verticaleLijst
-- 37,129-153
