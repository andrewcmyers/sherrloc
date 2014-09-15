module Html(module Pretty, module Html) where

import Pretty


data HTML = Tekst String
          | ET String [Attribuut]
          | DT String [Attribuut] [HTML]

type Attribuut = (AttrNm, Waarde)
type AttrNm = String
type Waarde = String
type Requirement = (String, String)

eqString      :: String -> String -> Bool 
eqString = undefined
eqMaybe :: (a -> a -> Bool) -> (Maybe a) -> (Maybe a) -> Bool
eqMaybe = undefined









validate :: [Requirement] -> HTML -> Maybe Requirement
validate lijst html = foldl combine Nothing (map (validateSingle html) lijst)
                            where combine rest req = case rest of
                                          (Just x) -> (Just x)
                                          Nothing  -> case req of
                                                    Nothing -> Nothing
                                                    (Just x)-> (Just x)


validateSingle ::  HTML -> Requirement -> Maybe Requirement
validateSingle html requirement = testEigenschap requirement "" html


testEigenschap :: Requirement -> String -> HTML -> Maybe Requirement
testEigenschap _ _ (Tekst _) = Nothing
testEigenschap (ouderV, kindV) ouder (DT kind _ html)
                 | (kind `eqString` kindV) && not(ouder `eqString` ouderV) = Just (ouderV, kindV)
                 | otherwise = testELijst (map (testEigenschap (ouderV, kindV) kind) html)
testEigenschap (ouderV, kindV) ouder (ET kind _)
                 | (kind `eqString` kindV) && not(ouder `eqString` ouderV) = Just (ouderV, kindV)
                 | otherwise = Nothing
testEigenschap _ _ _ = Nothing


testELijst :: [Maybe Requirement] -> Maybe Requirement
testELijst [] = Nothing
testELijst (x:xs) | eqMaybe eqRequirement x Nothing = testELijst xs
                   | otherwise = x


eqRequirement :: Requirement -> Requirement -> Bool
eqRequirement (ouder1,kind1) (ouder2, kind2) = eqString ouder1 ouder2 && eqString kind1 kind2





simpel :: HTML -> Bool
simpel (Tekst _) = True
simpel (ET _ _ )= True
simpel (DT _ _ html )= simpel [html]


reqLijst :: [Requirement]
reqLijst = [("LU","LI"), ("HTML", "BODY"), ("HTML","HEAD"), ("BODY", "TABLE"), ("BODY", "H"), ("BODY", "HR")]

test :: HTML
test = Tekst "Hallo"

test2 :: HTML
test2 = DT "html" []    [DT "head" [] [Tekst "Hallo"],
                         DT "body" [] [Tekst "Hallo2"]]
test3 :: HTML
test3 = DT "html" [("bgcolor","red")]
                                [DT "head" [] [Tekst "Hallo"],
                                 DT "body" [] [Tekst "Hallo2"]]

test4 :: HTML
test4 = DT "html" [("bgcolor","red")]
                                [DT "head" [] [Tekst "Hallo"],
                                 DT "body" [] [Tekst "Hallo2", ET "br" []]]

test5 :: HTML
test5 = DT "HTML" [("bgcolor","red")]
                                [DT "HEAD" [] [Tekst "Hallo", DT "TABLE" [] [ET "HR" []]],
                                 DT "BODY" [] [Tekst "Hallo2", ET "HR" []]

                                 ]

-- should be False
-- 68,24-36   68,31-36
