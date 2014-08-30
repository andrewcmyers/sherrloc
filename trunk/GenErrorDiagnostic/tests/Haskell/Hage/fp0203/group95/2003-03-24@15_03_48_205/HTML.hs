module HTML (module Pretty, module Maybe, module HTML) where

import Pretty
import Data.Maybe

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
chr :: Int -> Char
chr = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqTuple2 :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
eqTuple2 = undefined

data HTML
 = Text String
 | Tag  (String,[(String,String)])
 | Tags (String,[(String,String)]) [HTML] (String)



vbHTML :: HTML
vbHTML = Tags ("HTML",[])
         [Tag ("HEAD",[]),
         Tags ("BODY",[])
         [Tags("UL",[])
         [Tags("LI",[]) [Tags("FONT",[("COLOR","#0000FF")]) [Text "Eerste"] ("/FONT"),Text "Punt"] ("/LI"),
         Tag ("HR",[]),
         Tags("LI",[]) [Text "Tweede Punt"] ("/LI")]
         ("/UL")]
         ("/BODY")]
         ("/HTML")

type Requirement = (String,String)

validate :: [Requirement] -> HTML -> Maybe Requirement
validate [] _ = Nothing
validate (x:xs) html | (elemBy (eqTuple2 eqString eqString) x (valideer x html)) = Just x
                     | otherwise = validate xs html


valideer :: Requirement -> HTML -> [Requirement]
valideer (a,b) (Tags (open,[(_,_)]) html (_)) | ((not (eqString a open)) && (elemBy (eqString) b (map (check) html))) = [(a,b)]
                                                  | otherwise = concatMap (valideer (a,b)) html
valideer (a,b) (Tags (open,[]) html (_)) | ((not (eqString a open)) && (elemBy (eqString) b (map (check) html))) = [(a,b)]
                                             | otherwise = concatMap (valideer (a,b)) html
valideer (_,_) (Tag (_,[(_,_)])) = [("","")]
valideer (_,_) (Tag (_,[])) = [("","")]
valideer (_,_) (Text _) = [("","")]

check :: HTML -> String
check (Text _) = ""
check (Tag  (tag,[(_,_)])) = tag
check (Tag  (tag,[])) = tag
check (Tags (open,[(_,_)]) _ (_)) = open
check (Tags (open,[]) _ (_)) = open

simpel :: [HTML] -> Bool
simpel ((Text x):(Text xs)) = True




















color :: Int -> Int -> Int -> String
color r g b = (hex (r/16)):(hex (rem r 16)):(hex (g/16)):(hex (rem g 16)):(hex (b/16)):(hex (rem b 16)):[]

hex :: Int -> Char
hex n | (n < 10)  = chr (48+n)
      | (n >= 10) = chr (55+n)

-- (...) should be [...]
-- 59,19-25
