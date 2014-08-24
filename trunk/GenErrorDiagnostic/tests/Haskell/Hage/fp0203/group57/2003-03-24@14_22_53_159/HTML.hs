module HTML (module HTML, module Pretty) where
import Pretty

data HTML = Tekst String
          | EnkelTag String Attributes
          | Tag String Attributes [HTML]

type Attributes  = [(String,String)]
type Requirement = (String, String)

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool        :: Bool -> Bool -> Bool 
eqBool = undefined
chr :: Int -> Char
chr = undefined



val :: (String, String) -> HTML -> Bool
val (_, _)  (Tekst _) = True
val (x, y)  (EnkelTag t _) =  if eqString t x then eqString y "" else True
val (x, y)  (Tag t _ html) =  if eqString t x then elemBy eqBool True (map (val (y,x)) (html)) else False



simpel :: [HTML] -> Bool
simpel = all simpelelem

simpelelem :: HTML -> Bool
simpelelem x = case x of
                 Tekst _       -> True
                 EnkelTag _ _  -> True
                 Tag _ _ ys    -> if all simpelelem ys && length ys == 1 then True else False


ppHTML :: HTML -> Doc
ppHTML   html2  = case html2 of
                  Tekst u             -> tekst u
                  EnkelTag str []     -> tekst ( "<" ++ str ++ ">" )
                  EnkelTag str attr   -> tekst ("<" ++ str ++ " " ++ schrijfAttr attr ++ ">")
                  Tag str [] html     -> if simpel html then schrijf2 str html else schrijf1 str html
                  Tag str attr html   -> if simpel html then schrijf4 str attr html else schrijf3 str attr html


schrijf1 :: [Char] -> [HTML] -> Doc
schrijf1 str html  =    (<->)  (tekst( "<" ++ str ++ ">")) ((<->) (foldr (<->) leeg (map ppHTML html)) (tekst ("<" ++ "/" ++ str ++  ">")))

schrijf2 :: [Char] -> [HTML] -> Doc
schrijf2 str html  =    (<|>)  (tekst( "<" ++ str ++ ">")) ((<|>) (foldr (<|>) leeg (map ppHTML html)) (tekst ("<" ++ "/" ++ str ++  ">")))

schrijf3 :: [Char] -> Attributes -> [HTML] -> Doc
schrijf3 str attr html  =    (<->)(tekst ("<" ++ str ++ " " ++ schrijfAttr attr ++ ">")) ((<->) (foldr (<->) leeg (map ppHTML html)) (tekst ("<" ++ "/" ++ str ++  ">")))

schrijf4 :: [Char] -> Attributes -> [HTML] -> Doc
schrijf4 str attr html  =    (<|>) (tekst ("<" ++ str ++ " " ++ schrijfAttr attr ++ ">")) ((<|>) (foldr (<|>) leeg (map ppHTML html)) (tekst ("<" ++ "/" ++ str ++  ">")))



schrijfAttr :: Attributes -> [Char]
schrijfAttr attrs  = concat (map schrijf1Attr attrs)

schrijf1Attr ::  ([Char], [Char]) -> [Char]
schrijf1Attr attr = fst attr ++ "=" ++ "\"" ++ snd attr ++ "\""

color :: Int -> Int -> Int -> String
color x y z = "#" ++ decNaarHexdec x ++ decNaarHexdec y ++ decNaarHexdec z

decNaarHexdec :: Int -> String
decNaarHexdec x |  x `div` 16 >= 0 = [chr(x `div` 16 + 55 )]
                | otherwise = x





colorTable  = schrijf4 "Table" [("height","400"),("width","400")] []













colorKolom (x, y, z) = color x y z

-- x should be: [chr(x `div` 16 + 48 )]
-- 73,31-31
