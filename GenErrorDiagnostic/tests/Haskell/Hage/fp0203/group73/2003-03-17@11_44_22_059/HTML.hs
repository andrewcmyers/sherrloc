module HTML(module Pretty, module HTML) where

import Pretty

chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

data HTML
  = Singletag String [Attribute]
  | Doubletag String [Attribute] [HTML]
  | Text String

type Requirement = (String, String)
type Attribute = (String,String)

main       :: IO ()
main = layout(ppHTML(document "Kleurentabel" kleurenTabel))

vb1 :: HTML
vb1 = Doubletag "AL" []
       [
        Doubletag "LI" []
         [
          (Doubletag "FONT"
            [ ("COLOR","#0000FF")]
            [ Text "Eerste"]),
          (Text " punt")],
        Singletag "HR" [("blaat","mekker")],
        Doubletag "LI" []
        [Text "Tweede punt"]
        ]

vb2 :: HTML
vb2 = Doubletag "HTML" [] [Singletag "BR" [], Singletag "BR" [],vb1]


validate             :: [Requirement] -> HTML -> Maybe Requirement
validate [] _         = Nothing
validate reqs htmlDoc = checkList(map (\x-> validateone x htmlDoc) reqs)


validateone :: Requirement -> HTML -> Maybe Requirement
validateone _ (Text _)            = Nothing
validateone _ (Singletag _ _)    = Nothing
validateone _ (Doubletag _ _ []) = Nothing
validateone (tag1,tag2) (Doubletag name _ ((Text _):xs)) = checkList((validateone (tag1,tag2) (Doubletag name [] xs)):(map (validateone (tag1,tag2)) xs))
validateone (tag1,tag2) (Doubletag name _ ((Singletag name2 _):xs)) | (name2 `eqString` tag2) = if (name `eqString` tag1)
                                                                                                   then checkList((validateone (tag1,tag2) (Doubletag name [] xs)):(map (validateone (tag1,tag2)) xs))
                                                                                                   else Just (tag1,tag2)
                                                                    | otherwise = checkList((validateone (tag1,tag2) (Doubletag name [] xs)):(map (validateone (tag1,tag2)) xs))
validateone (tag1,tag2) (Doubletag name _ ((Doubletag name2 _ _):xs)) | (name2 `eqString` tag2) = if (name `eqString` tag1)
                                                                                                   then checkList((validateone (tag1,tag2) (Doubletag name [] xs)):(map (validateone (tag1,tag2)) xs))
                                                                                                   else Just (tag1,tag2)
                                                                      | otherwise = checkList((validateone (tag1,tag2) (Doubletag name [] xs)):(map (validateone (tag1,tag2)) xs))



checkList             :: [Maybe a] -> Maybe a
checkList []           = Nothing
checkList ((Just x):_) = Just x
checkList (_:xs)       = checkList xs


ppHTML                               :: HTML -> Doc
ppHTML (Text st)                      = tekst(st)
ppHTML (Singletag name attributes)    = tekst("<" ++ name ++ ppAttributes attributes ++ ">")
ppHTML (Doubletag name attributes xs) | isSimpleList (Doubletag name attributes xs) = tekst("<" ++ name ++ ppAttributes attributes ++ ">") <|> concat(concatMap (ppHTML) xs) <|> tekst("</" ++ name ++ "s>")
                                      | otherwise = tekst("<" ++ name ++ ppAttributes attributes ++ ">") <-> (springIn 1 (verticaleLijst (map (ppHTML) xs))) <-> tekst("</" ++ name ++ ">")













isSimpleList2 :: HTML -> Bool
isSimpleList2 (Text _)        = True
isSimpleList2 (Singletag _ _) = True
isSimpleList2 (Doubletag _ _ htmls) | (length htmls) == 0 = True
                                    | (length htmls) == 1 = and(map (isSimpleList) htmls)
                                    | otherwise = False

isSimpleList :: HTML -> Bool
isSimpleList (Singletag _ _) = True
isSimpleList (Text _) = True
isSimpleList (Doubletag _ _ htmls) = and(map (isSimpleList2) htmls)


ppAttributes                  :: [(String,String)] -> String
ppAttributes []                = ""
ppAttributes ((name,value):xs) = " " ++ name ++ "=\"" ++ value ++ "\"" ++ ppAttributes xs


colorTable   :: [[(Int, Int, Int)]] -> HTML
colorTable xs = Doubletag "TABLE" [("WIDTH","400"),("HEIGHT","400")] (map (writeColorRow) xs)


ul                :: [Attribute] -> [HTML] -> HTML
ul attributes html = Doubletag "UL" attributes html


h         :: Int -> String -> HTML
h size txt = Doubletag ("H" ++ siz) [] [Text txt]
              where siz = [chr (size + ord '0')]


font                 :: [Attribute] -> [HTML] -> HTML
font attributes htmls = Doubletag "FONT" attributes htmls


text    :: String -> HTML
text txt = Text txt


document :: String -> [HTML] -> HTML
document title html =  Doubletag "HTML" [] [headd,body]
               where tittle = Doubletag "TITLE" [] [Text title]
                     headd  = Doubletag "HEAD"  [] [tittle]
                     body   = Doubletag "BODY"  [] html



kleurenTabel :: [HTML]
kleurenTabel  =  [title,list,table]
          where title     = h 1 "Kleurentabel"
                list      = ul [] [tagRood, tagGroen, tagBlauw]
                tagRood   = Doubletag "LI" [] [rood,roodText]
                tagGroen  = Doubletag "LI" [] [groen,groenText]
                tagBlauw  = Doubletag "LI" [] [blauw,blauwText]
                rood      = font [("color",(color 255 0 0))][(text "Rood")]
                groen     = font [("color",(color 0 255 0))][(text "Groen")]
                blauw     = font [("color",(color 0 0 255))][(text "Blauw")]
                roodText  = text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                groenText = text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"
                blauwText = text "is overal 0"
                table     = colorTable geg
                geg       = groepeer 11 [(x,y,z) | x<-[0,25..250], y<-[0,25..250],z<-[0]]


writeColorRow  :: [(Int,Int,Int)] -> HTML
writeColorRow a = Doubletag "TR" [] (map (writeColorCel) a)

writeColorCel        :: (Int,Int,Int) -> HTML
writeColorCel (r,g,b) = Doubletag "TD" [("BGCOLOR",(color r g b))] []

groepeer     :: Int -> [a] -> [[a]]
groepeer _ [] = []
groepeer n xs = take n xs : groepeer n (drop n xs)

color               :: Int -> Int -> Int -> String
color red green blue = "#" ++ f red ++ f green ++ f blue
                     where f n = toHex n


toHex :: Int -> String
toHex num = rnd(num / 16) : [rnd(num `mod` 16)]
          where rnd n | n == 10 = 'A'
                      | n == 11 = 'B'
                      | n == 12 = 'C'
                      | n == 13 = 'D'
                      | n == 14 = 'E'
                      | n == 15 = 'F'
                      | n == 16 = 'G'
                      | otherwise = chr(n+48)

-- missing []
-- 71,144-172
