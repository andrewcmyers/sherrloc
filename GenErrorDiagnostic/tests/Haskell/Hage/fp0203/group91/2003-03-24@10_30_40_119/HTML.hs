module HTML(module Pretty, module HTML) where

import Pretty

eqString      :: String -> String -> Bool 
eqString = undefined
chr :: Int -> Char
chr = undefined
toUpper :: Char -> Char
toUpper = undefined

type Naam = String
type Tekst = String
type Attribuut = (String,String)

type Requirement = (Naam, Naam)

requirements :: [(String, String)]
requirements = [("UL", "LI")
               ,("HTML", "HEAD")
               ,("HTML", "BODY")
               ,("BODY", "HR")
               ,("BODY", "H")
               ,("BODY", "TABLE")
               ]

data HTML =  Tekst Tekst
          | EnkeleTag Naam [Attribuut]
          | Tag Naam [Attribuut] [HTML]








validate :: [Requirement] -> HTML -> Maybe Requirement
validate reqs html@(Tag naam _ _) = case length illegReqs of
                                        0 -> Nothing
                                        _ -> Just (head illegReqs)
                where illegReqs = [x | x<-reqs, not $ validate' x naam html]

validate' :: Requirement -> String -> HTML -> Bool
validate' _ _ (Tekst _) = True
validate' req ouder (EnkeleTag naam _) = eqString ouder (fst req) || (not . eqString naam $ snd req)
validate' req ouder (Tag naam _ htmls) = if (not . eqString ouder) (fst req) && (naam `eqString` snd req) then
                                                 False
                                         else all (validate' req naam) htmls

test :: HTML
test = Tag "HTML" [("a","b"),("c","d")] [
                     (
                      Tag "UL" [] [
                                   (Tag "LI" [] [(Tag "FONT" [("color","#FF00FF")] [Tekst "eerste"]), (Tekst "punt")  ])
                                   ,(EnkeleTag "HR" [])
                                   ,(Tag "LI" [] [Tekst "tweede punt"])
                                   ,(Tag "LI" [("style", "bullet"),("bla","blie")] [])
                                   ]
                      ), EnkeleTag "BR" []
                      ]















ppHTML :: HTML -> Doc
ppHTML (Tekst x) = tekst x
ppHTML (EnkeleTag naam attrs) = tekst ("<" ++ naam ++ geefAttrs attrs ++ ">")
ppHTML (Tag naam attrs htmls) = if isSimpel htmls then
                                    tekst ("<" ++ naam ++ geefAttrs attrs ++ ">") <|> (foldr (<|>) leeg (map ppHTML htmls)) <|>   tekst ("</" ++ naam ++ ">")
                                else
                                    tekst ("<" ++ naam ++ geefAttrs attrs ++ ">") <-> (springIn 1 (foldr (<->) leeg (map ppHTML htmls))) <->   tekst ("</" ++ naam ++ ">")


isSimpel :: [HTML] -> Bool
isSimpel [(Tekst _)] = True
isSimpel [(EnkeleTag _ _)] = True
isSimpel [(Tag _ _ htmls)] = case length htmls of
                              1 -> True
                              _ -> False
isSimpel (x:xs) = isSimpel [x] && isSimpel xs
isSimpel [] = True


geefAttrs :: [Attribuut] -> String
geefAttrs [] = ""
geefAttrs (x:xs) = map toUpper $ " " ++ fst x ++ "=" ++ "\"" ++ snd x ++ "\"" ++ geefAttrs xs






color :: Int -> Int -> Int -> String
color r g b = "#" ++ hexString r ++ hexString g ++ hexString b

hexString :: Int -> String
hexString waarde | pos1 > 9 && pos2 > 9 = map chr (pos1 + 55, pos2 + 55)
                where pos1 = waarde / 16
                      pos2 = waarde `mod` 16

-- (...) should be [...]
-- 109,51-72
