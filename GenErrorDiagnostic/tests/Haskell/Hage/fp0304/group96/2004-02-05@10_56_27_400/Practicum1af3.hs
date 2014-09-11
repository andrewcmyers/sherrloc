module Practicum1af3 where
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

aantal_spaties :: Int -> Int -> Int
aantal_spaties x y = y-x

schrijf_spaties :: Int -> String
schrijf_spaties 0 = []
schrijf_spaties x = " " ++ schrijf_spaties (x-1)

aanvullen :: String -> Int -> String
aanvullen _ 0 = []
aanvullen x y = x++schrijf_spaties (aantal_spaties (length x) y)

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




join_fix1 :: Table -> Table -> Table -> Table
join_fix1 ((x:xs):xss) ((y:ys):yss) z | eqString x y = (x:xs)
                                      | True         = join_fix1 (xs:xss) (ys:yss) z
join_fix1 ([]:xss) _ z = join_fix2 xss z
join_fix1 _ _ _ = []

join_fix2 :: Table -> Table -> Table
join_fix2 ((x:xs):xss) ((y:ys):yss) | eqString x y = (x:xs)
                                    | True         = join_fix2 xss ((y:ys):yss)
join_fix2 _ _ = []

-- missing []
-- 112,57-60   118,55-58
