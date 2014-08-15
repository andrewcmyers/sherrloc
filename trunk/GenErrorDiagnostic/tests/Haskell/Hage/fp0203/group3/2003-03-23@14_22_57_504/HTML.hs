module HTML(module HTML) where

type Requirement = (String, String)
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined
eqString      :: String -> String -> Bool 
eqString = undefined

data HTML = Tekst String
          | Tag String [Requirement]
          | Deelboom String [Requirement] [HTML]


vbHTML :: HTML
vbHTML     = Deelboom "UL" []
            [
             Deelboom "LI" [] [Deelboom "FONT" [("COLOR","#0000FF")] [Tekst "eerste"],Tekst "punt"],
             Tag "HR" [],
             Deelboom "LI" [] [Tekst "Tweede Punt"]
            ]

vbHTML2 :: HTML
vbHTML2 = Deelboom "UL" []
          [Deelboom "LI" [] [Deelboom "B" [] [Tekst "1"],Tekst "12"],
           Deelboom "LI" [] [Tekst "3"]
          ]

vbHTML3 :: HTML
vbHTML3 = Deelboom "TR" []
          [Deelboom "TD" [] [],
           Deelboom "TD" [] []
          ]

vbreq :: [Requirement]
vbreq = [("UL", "LI"),("HTML","BODY"),("HTML","HEAD"),("BODY","TABLE"),
         ("BODY","H"),("BODY","HR")]





validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (x:xs) html | singlevalidate False x html = Just x
                     | otherwise                   = validate xs html

singlevalidate :: Bool -> Requirement -> HTML -> Bool
singlevalidate _ _ (Tekst _) = False
singlevalidate b (_,onder) (Tag s _) = not b && eqString onder s
singlevalidate b tuple@(boven,onder) (Deelboom s _ x) =
               (not b && eqString onder s) ||
               or (map (singlevalidate (eqString boven s) tuple) x)





ppHTML :: HTML -> Doc
ppHTML html | simpel html = regelfunctie html
            | otherwise = inspringFunctie html


inspringFunctie :: HTML -> Doc
inspringFunctie (Deelboom tagname tuples htmls) = openTag tagname tuples :
                                                  springIn 1 (concatMap ppHTML htmls) ++
                                                  [sluitTag tagname]

regelfunctie :: HTML -> Doc
regelfunctie (Tekst s) = tekst s
regelfunctie (Tag s a) = tekst (openTag s a)
regelfunctie (Deelboom s tuples htmls) = [openTag s tuples ++ concat (concatMap regelfunctie htmls) ++ sluitTag s]

simpel :: HTML -> Bool
simpel (Tekst _)             = True
simpel (Tag _ _)             = True
simpel (Deelboom _ _ htmls)  = simpel1 htmls

simpel1 :: [HTML] -> Bool
simpel1 []          = True
simpel1 (Tekst _:xs) = simpel1 xs
simpel1 (Tag _ _:xs)= simpel1 xs
simpel1 (Deelboom _ _ [x]:xs) = simpel x && simpel1 xs
simpel1 _ = False

openTag :: String -> [Requirement] -> String
openTag tag att = '<' : tag ++ unwords (map (\x -> " "++(fst x)++"="++(snd x)) att) ++ ">"

sluitTag :: String -> String
sluitTag s = "</" ++ s ++ ">"




color :: Int -> Int -> Int -> String
color a b c = "#" ++ dec2hex a ++ dec2hex b ++ dec2hex c

dec2hex :: Int -> String
dec2hex x = [dec2char (x/16)] ++ [dec2char (x `mod` 16)]
dec2char :: Int -> Char
dec2char x | x <= 10   = chr (ord '0' + x)
             | x <= 16   = chr (ord 'A' + x-10)
             | otherwise = error "dec2hex input should be <256!"

colorTable :: [[(Int,Int,Int)]] -> HTML
colorTable x = Deelboom "TABLE" [("HEIGHT","400"),("WIDTH","400")] (colorRows x)

colorRows :: [[(Int,Int,Int)]] -> [HTML]
colorRows x = (map colorRow x)

colorRow :: [(Int,Int,Int)] -> HTML
colorRow x = Deelboom "TR" [] (map colorCell x)

colorCell :: (Int, Int, Int) -> HTML
colorCell (a,b,c) = Deelboom "TD" [("BGCOLOR",color a b c)] []

ul :: [[HTML]] -> HTML
ul htmls = Deelboom "UL" [] (map (\x->Deelboom "LI" [] x) htmls)

h :: Int -> String -> HTML
h a b = Deelboom ("H"++[chr(ord '0' + a)]) [] [(Tekst b)]

font :: [Requirement] -> [HTML] -> HTML
font a b = Deelboom "FONT" a b

text :: String -> HTML
text a = Tekst a

document :: String -> [HTML] -> HTML
document a b = Deelboom "HTML" []
               [  Deelboom "HEAD" [] [Deelboom "TITLE" [] [text a]]
                 ,Deelboom "BODY" [] b ]



kleurenTabel :: HTML
kleurenTabel = document "Kleurentabel"
               [h 1 "Kleurentabel",
                ul [[font [("COLOR",color 255 0 0)] [text "rood"],text "loopt van boven naar onder van 0 tot en met 250 in stappen van 25"],
                    [font [("COLOR",color 0 255 0)] [text "groen"],text "loopt van links naar rechts van 0 tot en met 250 in stappen van 25"],
                    [font [("COLOR",color 0 0 255)] [text "blauw"],text "is overal 0"]],
                colorTable [(zip3 (take 11 (iterate (+25) 0)) (replicate 11 0) (replicate 11 0))]
               ]

functie :: (Int,Int,Int) -> [(Int,Int,Int)]
functie (a,b,c) = zip3 (take 11 (iterate  (+25) 0)) b c


main :: IO ()
main = layout(ppHTML kleurenTabel)







infixl 4 <->
infixl 5 <|>






type Doc = [String]







leeg     :: Doc






tekst    :: String -> Doc






















(<|>)    :: Doc -> Doc -> Doc





















(<->)    :: Doc -> Doc -> Doc





springIn :: Int -> Doc -> Doc










layout   :: Doc -> IO ()




leeg = []


tekst string
  = [ string ]


[] <|> rechts
  = rechts
links <|> []
  = links
links <|> rechts
  = init links
    ++
    [ last links ++ head rechts ]
    ++
    springIn inspringing (tail rechts)
  where
    inspringing = length (last links)


boven <-> onder
  = boven ++ onder


springIn n regels
  = map spatiesErvoor regels
  where
    spatiesErvoor regel = spaties n ++ regel


layout doc
  = putStr (unlines doc)



spaties :: Int -> String
spaties n = replicate n ' '

infixl 4 <+>

(<+>) :: Doc -> Doc -> Doc
doc1 <+> doc2 = doc1 <|> springIn 1 doc2

kommaLijst :: [Doc] -> Doc
kommaLijst [] = leeg
kommaLijst [x] = x
kommaLijst (x:xs) = x <|> tekst "," <+> kommaLijst xs

haskellLijst :: [Doc] -> Doc
haskellLijst docs
  = tekst "[" <|> kommaLijst docs <|> tekst "]"

utrechtseLijst :: [Doc] -> Doc
utrechtseLijst []
  = tekst "[]"
utrechtseLijst (doc:docs)
  =  tekst "[" <+> doc
 <-> verticaleLijst (map kommaErvoor docs)
 <-> tekst "]"
  where
    kommaErvoor docc = tekst "," <+> docc

verticaleLijst :: [Doc] -> Doc
verticaleLijst = foldr (<->) leeg

-- b c should be: (replicate 11 b) (replicate 11 c)
-- 147,53-53   147,55-55
