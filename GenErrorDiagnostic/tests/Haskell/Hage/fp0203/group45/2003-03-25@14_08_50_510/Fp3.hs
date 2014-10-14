module Fp3(module Fp3, module List, module Pretty) where

import Data.List
import Pretty
showInt       :: Int -> String
showInt = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
data HTML = Tekst String
           | EnkelTag Naam [Attributen]
           | DubbelTag Naam [Attributen] [HTML]

type Naam = String
type Attributen = (String, String)

type Requirement = (String,String)




validate::[Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (xs:xss) html |  not (check xs False html) = Just xs
                       | otherwise = validate xss html


check :: Requirement -> Bool -> HTML -> Bool
check _ _ (Tekst _)       = True
check _ _ (EnkelTag _ _ ) = True
check (x,y) False (DubbelTag name atr html) | eqString name y  = False
                                            | otherwise = check (x,y) True (DubbelTag name atr html)

check (x,y) True (DubbelTag name _ html)   | eqString x name = and(map (check (x,y) True) html)
                                           | otherwise       = and(map (check(x,y) False) html)





ppHTML ::HTML -> Doc
ppHTML  (Tekst txt ) = tekst txt
ppHTML (EnkelTag name atr ) = tekst ("<" ++ name) <|> ppAttributes atr <|> tekst ">"
ppHTML (DubbelTag name atr html) |  and(map checkSimple html) = ppHTML (EnkelTag name atr) <|> (concatMap ppHTML html) <|> tekst  ("</" ++ name ++ ">")
                                 |  otherwise = tekst ("<" ++ name) <|> ppAttributes atr <|> tekst ">" <-> (foldr (<->) [] (map ppHTML html)) <-> tekst ("</"++name ++">")

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
convert num | num <16  = (showInt 0) ++ [("0123456789ABCDEF" !! num)]
convert num | num > 15 =  convert (num/16) ++ zonderNul (num `mod` 16)

zonderNul :: Int -> String
zonderNul num =   [("0123456789ABCDEF" !! num)]



colorTable :: [[(Int, Int, Int)]] -> HTML
colorTable lijst = (DubbelTag "Table" [("WIDTH","400"),("HEIGHT","400")] (concat[makeTRS lijst]) )


makeTDS::[(Int,Int,Int)] -> [HTML]
makeTDS [(x,y,z)]  = [(DubbelTag "TD" [("BGCOLOR",color x y z)] [])]
makeTDS ((x,y,z):xss) = (DubbelTag "TD" [("BGCOLOR",(color x y z))] []): makeTDS xss

makeTRS:: [[(Int,Int,Int)]] -> [HTML]
makeTRS [lijst] = [(DubbelTag "TR" [] (concat[(makeTDS lijst)]))]
makeTRS (lijst:rest) = (DubbelTag "TR"  [] (concat[makeTDS lijst])): makeTRS rest



ul :: [[HTML]] -> HTML
ul lijst = DubbelTag "UL" [] (map (DubbelTag "LI" []) lijst)

font :: [Attributen] -> [HTML] -> HTML
font atr [x] = (DubbelTag "FONT" atr [x])
font atr html  = (DubbelTag "FONT"  atr html )

h:: Int -> String -> HTML
h x str =  DubbelTag ("H" ++ (showInt x))  [] [text str]

text ::String -> HTML
text str = (Tekst str)

document :: String -> [HTML] -> HTML
document str html =  (DubbelTag "HTML" [] [(DubbelTag "HEAD" [] [(DubbelTag "Title" [] [(Tekst  str)])]),(DubbelTag "BODY" [] (concat[html]))])









kleurenTabel::HTML
kleurenTabel = document "newthing" [html ,(colorTable genList)]

genListRed :: (Int,Int,Int) -> [(Int,Int,Int)]
genListRed (r,g,b) = take 11 ((r,g,b) : genListGreen (r+25,g,b))

genListGreen :: (Int,Int,Int) -> [(Int,Int,Int)]
genListGreen (r,g,b) = take 11 ((r,g,b) : genListGreen (r,g+25,b))


genList :: [[(Int,Int,Int)]]
genList = groepeer 11 [(x,y,0)|x<-[0,25..250], y<-[0,25..250]]

groepeer :: Int -> [a] -> [[a]]
groepeer _ [] = []
groepeer n xs = take n xs : groepeer n (drop n xs)

main :: IO ()
main = layout(ppHTML(kleurenTabel))




html = [font [("color",color 21 22 0)](h 1 "dit is something about me !")]

-- should be: font [("color",color 21 22 0)][(h 1 "dit is something about me !")] 
-- 138,8-74    138,40-72
