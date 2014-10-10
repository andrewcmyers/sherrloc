module Praktikum3(module Pretty, module Praktikum3) where
import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
type Requirement = (String, String)
type Tag         = String
type Attribute   = (String,String)

data HTML = Text String
          | SingleTag Tag [Attribute]
          | ClosedTag Tag [Attribute] [HTML]



voldoet :: Requirement -> Bool -> HTML -> Bool
voldoet _ _ (Text _)                                      = True
voldoet _ _ (SingleTag _ _)                               = True
voldoet (buiten, binnen) False (ClosedTag naam attr html) | eqString binnen naam = False
                                                          | otherwise            = voldoet (buiten,binnen) True (ClosedTag naam attr html)

voldoet (buiten, binnen) _ (ClosedTag naam _ html)        | eqString naam buiten = and (map (voldoet (buiten, binnen) True) html)
                                                          | otherwise            = and (map (voldoet (buiten, binnen) False) html)

validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _                            = Nothing
validate (requirement:requirements) html | not (voldoet requirement False html) = Just requirement
                                         | otherwise = validate requirements html

isSimpel :: HTML -> Bool
isSimpel (Text _)             = True
isSimpel (SingleTag _ _)      = True
isSimpel (ClosedTag _ _ html) = and (map isEnkel html)

isEnkel :: HTML -> Bool
isEnkel (Text _)            = True
isEnkel (SingleTag _ _)     = True
isEnkel (ClosedTag _ _ [])  = True
isEnkel (ClosedTag _ _ [_]) = True
isEnkel _                   = False

ppHTML :: HTML -> Doc
ppHTML (Text string)              = [enkeleRegel (Text string)]
ppHTML (SingleTag naam attr)      = [enkeleRegel (SingleTag naam attr)]
ppHTML (ClosedTag naam attr html) | isSimpel (ClosedTag naam attr html) = [enkeleRegel (ClosedTag naam attr html)]
                                  | otherwise = ["<" ++ naam ++ ">"]
                                                <->
                                                springIn 1 (foldl (<->) [] (map ppHTML html))
                                                <->
                                                ["</" ++ naam ++ ">"]

enkeleRegel :: HTML -> String
enkeleRegel (Text string)              = string
enkeleRegel (SingleTag naam atts)      = "<" ++ naam ++ (concatMap geefAtt atts) ++ ">"
                                       where geefAtt (x,y) = " " ++ x ++ "=" ++ y
enkeleRegel (ClosedTag naam atts html) = enkeleRegel (SingleTag naam atts) ++ concatMap enkeleRegel html ++ "</" ++ naam ++ ">"



intToHexHelper :: Int -> String
intToHexHelper num | num >= 0 && num <= 9   = [chr(num + (ord '0'))]
                   | num >= 10 && num <= 15 = [chr((num - 10) + (ord 'A'))]
                   | otherwise              = intToHexHelper(num/16) ++ intToHexHelper(rem num 16)

intToHex :: Int -> String
intToHex num | num >= 0 && num <= 9 = '0' : intToHexHelper num
             | otherwise            = intToHexHelper num


color :: Int -> Int -> Int -> String
color kleur1 kleur2 kleur3 = "#" ++ intToHex kleur1 ++ intToHex kleur2 ++ intToHex kleur3

intToString :: Int -> String
intToString = map(\x->chr(x+ord '0')).reverse.map(`mod` 10).takeWhile(/=0).iterate(/10)




colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable rgbs = ClosedTag "TABLE" [("WIDTH","400"),("HEIGHT","400")] (colorGeheel rgbs (length rgbs) (maximum(map length rgbs)))

colorGeheel :: [[(Int, Int, Int)]] -> [HTML]
colorGeheel [] = []
colorGeheel (rgb:rgbs) = ClosedTag "TR" [] (colorRegel rgb) : colorGeheel rgbs

colorRegel :: [(Int, Int, Int)] -> [HTML]
colorRegel [] = []
colorRegel ((r, g, b):rgbs) = ClosedTag "TD" [("BGCOLOR",color r g b)] [] : colorRegel rgbs







h :: Int -> String -> HTML
h getal string = ClosedTag ("H" ++  [chr(getal + ord '0')]) [] [(Text string)]

text :: String -> HTML
text string = Text string

-- should be: (colorGeheel rgbs)
-- 83,73-129   83,73-130
