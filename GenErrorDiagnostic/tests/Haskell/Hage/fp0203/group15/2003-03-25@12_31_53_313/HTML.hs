module HTML where
data HTML = Tekst String
          | Tag String [Attribute]
          | DTag String [Attribute] [HTML]
type Attribute = (String, String)

vb :: HTML
vb = DTag "HTML" [("Color","#95561D")] [(DTag "HEAD" [] [(DTag "BODY" [] [])]), (Tekst "blabla")]

eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
showInt       :: Int -> String
showInt = undefined

type Requirement =(String, String)

validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (x:xs) html | hulpvalidatie x html "" = Just x
                     | otherwise = validate xs html



hulpvalidatie :: Requirement -> HTML -> String -> Bool
hulpvalidatie _ (Tekst _) _ = False
hulpvalidatie (r1,r2) (Tag naam _ ) old | (eqString r2 naam) = not (eqString r1 old)
                                        | otherwise          = False
hulpvalidatie (r1,r2) (DTag naam _ html)old |(eqString r2 naam) = not (eqString r1 old)
                                            | otherwise = (or . (map (\x -> hulpvalidatie (r1,r2) x naam))) html


requirements :: [Requirement]
requirements = [("HTML","BODY"),("HTML","HEAD"),("UL","LI"),("BODY","TABLE"),("BODY","H"),("BODY","HR")]













color      :: Int -> Int -> Int -> String
color  a b c    = "#" ++ hexa a ++ hexa b ++ hexa c

hexa  :: Int -> String
hexa getal  = omschakelen(getal/16) ++ omschakelen(getal`rem`16)

omschakelen :: Int -> String
omschakelen g |  g >= 10 = chr(ord 'A' - 10 + g):[]
              | otherwise = showInt g
















h          :: Int -> String -> HTML
h size t   = DTag  "H"++ showInt size [] [Tekst t]

font       :: [Attribute] -> [HTML] -> HTML
font attribute html = DTag "FONT" attribute html

text       :: String -> HTML
text t     = Tekst t

-- missing () around "H"++ showInt size
-- 77,20-37   77,14-50
