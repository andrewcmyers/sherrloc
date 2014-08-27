module Similarity where

elemBy        :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy = undefined
eqString      :: String -> String -> Bool 
eqString = undefined
eqBool      :: Bool -> Bool -> Bool 
eqBool = undefined
eqChar      :: Char -> Char -> Bool 
eqChar = undefined
toUpper     :: Char -> Char
toUpper = undefined

similar :: String -> String -> Bool
similar name1' name2'
    | length name1 <= 1 || length name2 <= 1
        = False
    | otherwise =
        name1 `eqString` name2
        ||
        oneDiff name1 name2
        ||
        oneMore name1 name2
        ||
        oneMore name2 name1
        ||
        elemBy eqString name1 (swap name2)
    where
        name1 = map toUpper name1'
        name2 = map toUpper name2'

oneMore :: String -> String -> Bool
oneMore xs ys =
    length xs - length ys == 1
    &&
    or (map (eqString ys) (dropOne xs))

dropOne :: String -> [String]
dropOne []     = []
dropOne (x:xs) = xs : map (x:) (dropOne xs)

oneDiff :: String -> String -> Bool
oneDiff xs ys =
    length xs == length ys
    &&
    length (filter (eqBool True) (zipWith eqChar xs ys)) == length xs - 1

swap :: [a] -> [[a]]
swap [_] = []
swap (x:y:xs) = (y:x:xs) : map (x:) (swap (y:xs))
