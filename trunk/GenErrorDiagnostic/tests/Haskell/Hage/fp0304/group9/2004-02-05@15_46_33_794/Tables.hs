module Tables where

type Table = [[String]]
type Column = [String]
type Cname = String

eqString      :: String -> String -> Bool 
eqString = undefined

getname :: Column -> String

getname (x:xs) = x

removeFromTable :: String -> Table -> Table

removeFromTable x [] = []

removeFromTable n (x:xs) | n (eqString) (getname x)  = xs
                         | otherwise      = (x:xs)

-- should be: eqString n (getName x)
-- 18,28-28   18,28-51
