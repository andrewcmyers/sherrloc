module HTML(module HTML, module Pretty) where

import Pretty
eqString      :: String -> String -> Bool 
eqString = undefined


data HTML   = DubTag String [Atr] [HTML]
            | EnkTag String [Atr]
            | Txt String


type Atr    = (String,String)
type Requirement = (String, String)










voorbeeld :: HTML
voorbeeld = DubTag "HTML" [] [
              (DubTag "HEAD" [] [] ),
              (DubTag "BODY" [] [
                      (DubTag "UL" []
                                [(DubTag "LI" [] [(DubTag "FONT" [("COLOR","#0000FF")] [Txt "Eerste"] ), (Txt "Punt")] ),
                                 (EnkTag "HR" []),
                                 (DubTag "LI" [] [(Txt "Tweede Punt")] )]
                       )]
               )]















validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _                                         = Nothing
validate (x:xs) boom  | validateProc boom x False     = validate xs boom
                      | otherwise                     = Just x

validateProc :: HTML -> Requirement -> Bool -> Bool
validateProc (Txt _)               _     _              = True
validateProc (EnkTag naam _)       (_,y) bool           | bool           = True
                                                        | otherwise      = not (naam `eqString` y)
validateProc (DubTag naam _ lijst) reqs@(x,y) bool      = case ((y `eqString` naam),(x `eqString` naam),bool)
                                                               of (True,_,_)           -> bool
                                                                  (False,True,False)   -> foldr (&&) True (map (\kop -> validateProc kop reqs True) lijst)
                                                                  (False,False,_)      -> foldr (&&) True (map (\kop -> validateProc kop reqs bool) lijst)





foutVoorbeeld :: HTML
foutVoorbeeld = DubTag "HTML" [] [
                  (DubTag "HEAD" [] [] ),
                  (DubTag "BODY" [] [
                          (DubTag "LI" []
                                    [(DubTag "UL" [] [(DubTag "FONT" [("COLOR","#0000FF")] [Txt "Eerste"] ), (Txt "Punt")] ),
                                     (EnkTag "HR" []),
                                     (DubTag "Ul" [] [(Txt "Tweede Punt")] )]
                          )]
                   )]




req1 :: Requirement
req1 = ("UL","LI")

req2 :: Requirement
req2 = ("HTML","BODY")

req3 :: Requirement
req3 = ("HTML","HEAD")

req4 :: Requirement
req4 = ("BODY","TABLE")

req5 :: Requirement
req5 = ("BODY","H")

req6 :: Requirement
req6 = ("BODY","HR")

allReq :: [Requirement]
allReq = [req1,req2,req3,req4,req5,req6]

ppHTML :: HTML -> Doc
ppHTML html = toDoc html

toDoc :: HTML -> Doc
toDoc (Txt string)                                             = tekst string
toDoc (EnkTag naam atr)                                        = (atributeer naam atr)
toDoc (DubTag naam atr [])                                     = (atributeer naam atr) <|> (tekst ("</" ++ naam ++ ">"))
toDoc (DubTag naam atr htmls)           | all simpel htmls     = (atributeer naam atr) <|> verticaleLijst map toDoc htmls  <|> (sluittag naam)
                                        | otherwise            = (atributeer naam atr) <-> springIn 1 (concatMap toDoc htmls)  <-> (sluittag naam)
                                                        where sluittag   n    = tekst ("</" ++ n ++ ">")

atributeer :: String -> [Atr] -> Doc
atributeer n a = tekst ("<" ++ n ++ "" ++ (concatMap atrString a) ++ ">")

atrString :: Atr -> String
atrString (naam,waarde) = naam ++ "=" ++ "\"" ++ waarde ++ "\""

simpel :: HTML -> Bool
simpel (Txt _)                                 = True
simpel (EnkTag _ _)                            = True
simpel (DubTag _ _ kind)                       = ((length kind) == 1) && and (map simpel kind)

-- should be: concatMap toDoc htmls
-- 110,92-121
