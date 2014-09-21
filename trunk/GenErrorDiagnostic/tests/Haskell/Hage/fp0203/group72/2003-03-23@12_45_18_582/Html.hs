module Html(module Pretty, module Html) where

import Pretty

eqString      :: String -> String -> Bool 
eqString = undefined
eqChar      :: Char -> Char -> Bool 
eqChar = undefined
chr :: Int -> Char
chr = undefined
ord :: Char -> Int
ord = undefined

data Html =
     Tekst String
     | Tag String [Attribute]
     | Tag2 String [Attribute] [Html]

type Attribute = (String,String)



type Requirement = (String, String)


validate :: [Requirement] -> Html -> Maybe Requirement
validate lijstR html | and (lijstValids lijstR html) = Nothing
                     | otherwise = Just (head (filter  (not.f)  lijstR ))
                                 where f x = validate1Req x html

lijstValids :: [Requirement] -> Html -> [Bool]
lijstValids (x:xs) html = validate1Req x html : lijstValids xs html
lijstValids [] _ = []

validate1Req :: Requirement -> Html -> Bool
validate1Req (ouder, kind) html = test (ouder,kind) "" html

test :: Requirement -> String -> Html -> Bool
test _  _ (Tekst _) = True
test (ouder, kind) oudertag (Tag2 tagnaam _ htmllijst )  | tagnaam `eqString` kind = oudertag `eqString` ouder
                                                                          | otherwise = and (map (test (ouder,kind) tagnaam) htmllijst)
test (ouder,kind) oudertag (Tag tagnaam _) | tagnaam `eqString` kind = oudertag `eqString` ouder
                                                | otherwise = True







ppHTML :: Html -> Doc

ppHTML (Tekst string)= tekst string
ppHTML (Tag naam lijstA)= tekst( "<" ++ naam ++ schrijf lijstA ++ ">")
ppHTML (Tag2 naamTag lijstA lijstH) | simpel lijstH = schrijfBeginTag naamTag lijstA <|>  represent lijstH <|> schrijfEindTag naamTag
                                      | otherwise =  lastigPrint (Tag2 naamTag lijstA lijstH )

schrijfBeginTag :: [Char] -> [Attribute] -> Doc
schrijfBeginTag naamTag lijstA = tekst ("<" ++ naamTag ++ schrijf lijstA ++  ">")
schrijfEindTag :: [Char] -> Doc
schrijfEindTag naamTag = tekst ("<" ++ "/" ++ naamTag ++ ">")

represent :: [Html] -> Doc
represent (x:xs) =  ppHTML x <|> represent xs
represent [] = leeg

simpel :: [Html] -> Bool
simpel lijstH | length lijstH == 1 = True
              | otherwise = False

schrijf :: [Attribute] -> String
schrijf lijstA | null lijstA = ""
               |otherwise =  " " ++ concat(zipWith f (neemEersteVTupels lijstA) (neemTweedeVTupels lijstA))
               where f x y = x ++ "=" ++ y ++ " "
neemEersteVTupels :: [Attribute] -> [String]
neemEersteVTupels lijstA = map fst lijstA
neemTweedeVTupels :: [(a, b)] -> [b]
neemTweedeVTupels lijstA = map snd lijstA

lastigPrint :: Html -> Doc









lastigPrint (Tekst string)= tekst string
lastigPrint (Tag naam lijstA)= tekst( "<" ++ naam ++ schrijf lijstA ++ ">")
lastigPrint (Tag2 naamTag lijstA lijstH) = schrijfBeginTag naamTag lijstA <->  schrijfMooi  lijstH  <-> schrijfEindTag naamTag

schrijfMooi :: [Html] -> [String]
schrijfMooi lijstH  = foldl (<->) leeg (map (springIn 1) (map ppHTML lijstH))
schrijfMooi [] = leeg










color :: Int -> Int -> Int -> String






























color int1 int2 int3 = "#" ++ vereenvoudig (reken int1)  ++ vereenvoudig (reken int2) ++ vereenvoudig (reken int3)

vereenvoudig :: [Char] -> [Char]
vereenvoudig (x:xs) | x `eqChar` (head xs) = [x]
                    | otherwise = (x:xs)

reken :: Int -> [Char]
reken int | (e < 10)  && (d < 10) =  [digitChar e,digitChar d]
          | (e < 10) && (d >= 10) = [digitChar e, zetNaarLetter d]
          | (e >= 10) && (d < 10) = [zetNaarLetter e, digitChar d]
          | otherwise = [zetNaarLetter e, zetNaarLetter d]
        where e = int `div` 16
              d = int `mod` 16

zetNaarLetter :: Int -> Char
zetNaarLetter int | int == 10 = 'A'
                  | int == 11 = 'B'
                  | int == 12 = 'C'
                  | int == 13 = 'D'
                  | int == 14 = 'E'
                  | int == 15 = 'F'

digitChar :: Int -> Char
digitChar int = chr (int + ord '0')


colorTable lijst = Tag2 "TABLE" [("HEIGHT", "400"),("WIDTH","400")] (map colorRow lijst)
colorRow lijst =  Tag2 "TR" [] (map colorCell lijst)
colorCell (int1, int2, int3) = Tag2 "TD" [("BGCOLOR", color int1 int2 int3)] []


ul ::  [[Html]] -> Html
ul  lijstH = Tag2 "UL" [] (map f lijstH)
             where  f x = Tag2 "LI" x

h :: Int -> String -> Html
h int string = Tag2 ("H" ++ [(digitChar int)]) [] [Tekst string]

-- should be: f x = Tag2 "LI" [] x
-- 171,21-37    171,27-37
