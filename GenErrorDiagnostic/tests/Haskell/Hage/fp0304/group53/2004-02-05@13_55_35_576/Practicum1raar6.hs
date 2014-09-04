module Practicum1raar6 where
import Data.List

type Table = [[String]]

eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined

compilers :: Table
compilers =
  [ ["Compiler", "Universiteit/bedrijf"]
  , ["Helium", "Universiteit van Utrecht"]
  , ["NHC", "University of York"]
  , ["GHC", "Microsoft Research"]
  , ["Hugs", "Galois Connections"]
  , ["Hugs.NET", "Galois Connections"]
  , ["O'Haskell", "Oregon Graduate Institute"]
  , ["O'Haskell", "Chalmers University of Technology"]
  , ["HBC", "Chalmers University of Technology"]
  ]

locaties :: Table
locaties =
  [ ["Universiteit/bedrijf", "Land", "Stad"]
  , ["Universiteit van Utrecht", "Nederland", "Utrecht"]
  , ["University of York", "Engeland", "York"]
  , ["Microsoft Research", "Engeland", "Cambridge"]
  , ["Galois Connections", "Verenigde Staten", "Beaverton"]
  , ["Oregon Graduate Institute", "Verenigde Staten", "Beaverton"]
  , ["Chalmers University of Technology", "Zweden", "Goteborg"]
  ]





writeTable :: Table -> String
writeTable []     = []
writeTable (x:xs) = unlines[writePlusMin (x:xs),writeHead (transpose (x:xs)),writePlusMin (x:xs)] ++ writeTail xs (repeat(lengtelangste(x:xs))) ++ writePlusMin (x:xs)




writePlusMin :: Table -> String
writePlusMin []     = []
writePlusMin x      = plussen_en_minnen (length(transpose x)) (lengtelangste x)


writeHead :: Table -> String
writeHead ([] : _)     = []
writeHead []           = "|"
writeHead ((x:xs):xss) = '|' : aanvullen x (head (lengtelangste (transpose ((x:xs):xss)))) ++ writeHead xss


writeTail :: Table -> [[Int]] -> String
writeTail ([] : xss) ([] : yss)     = "|\n" ++ writeTail xss yss
writeTail ((x:xs):xss) ((y:ys):yss) = '|' : aanvullen x y ++ writeTail (xs:xss) (ys:yss)
writeTail _ _ = []


lengthlist :: [[a]] -> [Int]
lengthlist []           = []
lengthlist ([]:y)       = lengthlist y
lengthlist ((x:xs):xss) = length(x:xs) : lengthlist xss



lengtelangste :: Table -> [Int]
lengtelangste x = map maximum (map lengthlist(transpose x))


schrijf_spaties :: Int -> String
schrijf_spaties 0 = []
schrijf_spaties x = " " ++ schrijf_spaties (x-1)


aanvullen :: String -> Int -> String
aanvullen _ 0 = []
aanvullen x y = x++schrijf_spaties (y - length x)


minnen :: Int -> String
minnen 0 = []
minnen x = '-': minnen(x-1)


plussen_en_minnen :: Int -> [Int] -> String
plussen_en_minnen 0 []     = "+"
plussen_en_minnen _ []     = []
plussen_en_minnen x (a:as) = '+': minnen a ++ plussen_en_minnen (x-1) as




project :: [String] -> Table -> Table
project x y =  transpose (project_fix x (transpose y) (transpose y))



project_fix :: [String] -> Table -> Table -> Table
project_fix (x:xs) ((y:ys):yss) z | eqString x y = (y:ys) : project_fix xs z z
                                  | True         = project_fix (x:xs) yss z
project_fix _ _ _ = []




select :: String -> (String -> Bool) -> Table -> Table
select x functie z = tail (head (zet_kolom_voor_tabel [x] z)) : select_fix functie (tail (zet_kolom_voor_tabel [x] z))


select_fix :: (String -> Bool) -> Table -> Table
select_fix functie ((z:zs):zss) | eqBool (functie z) True = zs : select_fix functie zss
                                | True                    = select_fix functie zss

select_fix _ ([] : _) = error "error"
select_fix _ []       = []

zet_kolom_voor_tabel :: [String] -> Table -> Table
zet_kolom_voor_tabel x y = transpose (transpose (project x y) ++ transpose y)







plak_alle_rijen t1 t2 = [x++y | x<-t1, y<-t2, pred x y]
                   where gemveld = gemVeld (head t1) (head t2)
                         index1  = zoekIndex (head t1) 0
                         index2  = zoekIndex (head t2) 0
                         zoekIndex (x:xs) t | eqString gemveld x = t
                                            | otherwise = zoekIndex xs (t+1)
                         zoekElem (x:xs) 0  = x
                         zoekElem (x:xs) t  = zoekElem xs (t-1)
                         verwijderElem (x:xs) 0 = xs
                         verwijderElem (x:xs) t = x:verwijderElem xs
                         pred l1 l2 = eqString (zoekElem l1 index1) (zoekElem l2 index2)


gemVeld :: [String] -> [String] -> String
gemVeld l1 l2 = head [x | x  <- l1, y <- l2, eqString x y]

-- should be: x:verwijderElem xs (t-1)
-- 139,53-68   139,53-65
