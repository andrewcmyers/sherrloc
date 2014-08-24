module HTML (module HTML, module Maybe,module Pretty) where

import Data.Maybe
import Pretty

data HTML = Text String
          | Enkel String [Attribuut]
          | Dubbel String [Attribuut] [HTML]

data TagSoort = Open | Sluit

type Attribuut = (String, String)
type Requirement = (String, String)
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

htmlReqs :: [ Requirement]
htmlReqs = [ ("UL", "LI")
           , ("HTML", "BODY")
           , ("HTML", "HEAD")
           , ("BODY", "TABLE")
           , ("BODY", "H")
           , ("BODY", "HR")
           ]

validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html | null alleFouten = Nothing
                   | otherwise = Just (head alleFouten)
         where alleFouten = catMaybes [ valEnkel req html | req <- reqs]

valEnkel :: Requirement -> HTML -> Maybe Requirement
valEnkel (req1, req2) (Dubbel naam _ lijst)
         | or(map vglNaam lijst) && not(req1 `eqString` naam) = Just (req1,req2)
         | null subFouten = Nothing
         | otherwise = Just (head subFouten)
                     where vglNaam (Dubbel subNaam _ _) = eqString req2 subNaam
                           vglNaam (Enkel subNaam _)    = eqString req2 subNaam
                           vglNaam _              = False
                           subFouten = catMaybes (map (valEnkel (req1, req2)) lijst)
valEnkel _ _ = Nothing


ppHTML     :: HTML -> Doc
ppHTML (Text inhoud) = tekst inhoud
ppHTML (Enkel naam attrs) = ppTag Open naam attrs
ppHTML html@(Dubbel naam attrs lijst)
        | isSimpel html = ppTag Open naam attrs <|> (ppLijst (<|>) lijst) <|> ppTag Sluit naam []
        | otherwise     = ppTag Open naam attrs <->
                              springIn 1 (ppLijst (<->) lijst) <-> ppTag Sluit naam []

ppTag :: TagSoort -> String -> [Attribuut] -> Doc
ppTag Open  naam attrs = tekst ('<'  :  naam ++ attr2String attrs ++ ">")
ppTag Sluit naam _     = tekst ("</" ++ naam ++ ">")

attr2String :: [Attribuut] -> String
attr2String []    = []
attr2String attrs = ' ' : unwords (map zetOm attrs)
        where zetOm (naam, waarde) = naam ++ "=\"" ++ waarde ++ "\""


ppLijst :: (Doc -> Doc -> Doc) -> [HTML] -> Doc
ppLijst op lijst = foldr1 op (map ppHTML lijst)


isSimpel :: HTML -> Bool
isSimpel (Dubbel _ _ lijst) = and(map f lijst)
       where f elem@(Dubbel _ _ lijst2) = length lijst2 == 1 && isSimpel elem
             f _                        = True
isSimpel (Enkel _ _)               = True
isSimpel (Text _)                  = True

color :: Int -> Int -> Int -> String
color rood groen blauw  = '#' : bI2H rood ++ bI2H blauw ++ bI2H groen


bI2H :: Int -> String
bI2H n = f n : [g n]
      where f x = toChar (x / 16)
            g x = toChar (x `mod` 16)
            toChar x | x > 9 = chr(x+ ord 'a' - 10)
                     | otherwise = chr(x + ord '0')


colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable kleuren = Dubbel "TABLE" [("WIDTH", "400"), ("HEIGHT", "400")] (map colorRow kleuren)

colorRow :: [(Int, Int, Int)] -> HTML
colorRow kleuren = Dubbel "TR" [] (map colorCell kleuren)

colorCell :: (Int, Int, Int) -> HTML
colorCell (rood, groen, blauw) = Dubbel "TD" [("BGCOLOR", color rood groen blauw)]

-- the last parameter [] is missing
-- 95,34-82
