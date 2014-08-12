module Html(module List, module Html) where

import Data.List
import Pretty

data HTML =  MonoTag Tag [Attribuut] [HTML]
            |DuoTag  Tag [Attribuut] [HTML]
            |Tekst String

type Attribuut = (String,String)
type Tag = String

pagina::HTML
pagina = (DuoTag "UL" []
           [(DuoTag "LI" [][(DuoTag "FONT" [("COLOR","#0000FF")][Tekst "punt"])])

            ,(MonoTag "HR"[][])
            ,(DuoTag "LI" [][(Tekst "Tweede punt")])

           ]
         )










ppComplex:: HTML->Doc
ppComplex tag = case tag of
               (MonoTag t attbs htm )  -> tekst ('<':t)
                                          <|> (foldr (\ a b-> [plakStringTupel a] <+> b)  leeg attbs)
                                          <|>tekst ">" <+>(foldr (\ a b-> ppHTML a <+> b)  leeg htm)

               (DuoTag  t attbs htm ) ->   tekst ('<':t)
                                           <|> (foldr (\ a b-> [plakStringTupel a] <+> b)  leeg attbs)
                                           <|>tekst ">" <-> tekst " "
                                           <+>(foldr (\ a b-> ppHTML a <-> b)leeg htm)
                                           <-> tekst("</"++ t ++">")

               (Tekst s)               -> [s]


ppSimpel:: HTML->Doc
ppSimpel tag = case tag of
               (MonoTag t attbs htm )  -> tekst ('<':t)
                                          <|> (foldr (\ a b-> [plakStringTupel a] <+> b)  leeg attbs)
                                          <|>tekst ">" <+>(foldr (\ a b-> ppHTML a <+> b)  leeg htm)

               (DuoTag  t attbs htm ) ->   tekst ('<':t)
                                           <|> (foldr (\ a b-> [plakStringTupel a] <+> b)  leeg attbs)
                                           <|>tekst ">" <-> tekst " "
                                           <+>(foldr (\ a b-> ppHTML a <-> b)leeg htm)
                                           <-> tekst("</"++ t ++">")

               (Tekst s)               -> [s]


ppHTML:: HTML->Doc
ppHTML html | isSimpel html = ppSimpel
            | True          = ppComplex



isSimpel:: HTML->Bool
isSimpel(DuoTag _ _ htm)|length htm == 1                          = True
                        |foldr (\ a b-> isSimpel a && b) True htm = True
                        |True                                     = False




plakStringTupel :: (String,String)->String
plakStringTupel (f,s) = f++"= \""++ s ++ "\""


hoi = layout(ppHTML pagina)

-- missing html for both functions
-- 63,31-38  64,31-39
