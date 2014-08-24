module Fp3(module Fp3, module List, module Pretty) where

import Data.List
import Pretty

data HTML = Tekst String
           | EnkelTag Naam [Attributen]
           | DubbelTag Naam [Attributen] [HTML]

type Naam = String
type Attributen = (String, String)

type Requirement = (String,String)

eqString      :: String -> String -> Bool 
eqString = undefined
validate::[Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (xs:xss) html |  not (check xs False html) = Just xs
                       | otherwise = validate xss html


check :: Requirement -> Bool -> HTML -> Bool
check _ _ (Tekst _)       = True
check _ _ (EnkelTag _ _ ) = True
check (x,y) False (DubbelTag name atr html) | eqString name y  = False
                                         | otherwise = check (x,y) True (DubbelTag name atr html)

check (x,y) True (DubbelTag name _ html) | eqString x name = and(map (check (x,y) True) html)
                                           | otherwise       = and(map (check(x,y) False) html)





ppHTML ::HTML -> Doc
ppHTML  (Tekst txt ) = tekst txt
ppHTML (EnkelTag name atr ) = tekst ("<" ++ name) <|> ppAttributes atr <|> tekst ">"
ppHTML (DubbelTag name atr html) |  and(map checkSimple html) = ppHTML (EnkelTag name atr) <|> (concatMap ppHTML html) <|> tekst  ("</" ++ name ++ ">")
                                 |  otherwise = tekst ("<" ++ name) <|>ppAttributes atr <|> tekst ">" <-> (foldl (<->) [] (map ppHTML html)) <-> tekst ("</"++name ++">")

checkSimple ::HTML -> Bool
checkSimple (Tekst _) = True
checkSimple (EnkelTag _ _) = True
checkSimple (DubbelTag _ _ html) = if length html <=1 then True else False




ppAttributes::[Attributen] -> Doc
ppAttributes atr  =tekst (concat(map attach  atr))
                     where attach (x,y) =" " ++ x ++ "=" ++ y ++ " "








color :: Int -> Int -> Int -> String
color x y z  = "#"++  convert x ++ convert y ++ convert z

convert::Int ->  String
convert num | num <16  = [("0123456789ABCDEF" !! num)]
convert num  = convert (num/16) ++ convert (num `mod` 16)

colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable (color:colors) = (DubbelTag "Table" [convertAttributes color] [colorTable colors])


convertAttributes::[(Int,Int,Int)] -> [Attributen]
convertAttributes  [(x,y,z)] = [("Width","40"),("Height","40"),("Bgcolors",color x y z)]








text ::String -> HTML
text str = (Tekst str)

-- should be (concat[convertAttributes color]) [colorTable colors])
-- 69,49-71
