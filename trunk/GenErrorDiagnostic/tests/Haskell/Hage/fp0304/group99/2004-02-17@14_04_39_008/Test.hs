module Test where

showInt       :: Int -> String
showInt = undefined

test = do
       naam <- getLine
       putStr (showInt length(naam))

-- missing () around length(naam)
-- 8,16-35   8,16-22
