module Oefentoets3 where

eqInt      :: Int -> Int -> Bool
eqInt = undefined

lastIndexOf                                 :: Int -> [Int] -> Int
lastIndexOf  n lijst | eqInt n (last lijst) = (length lijst) -1
                     | otherwise            = lastIndexOf n (init lijst)


lastIndexOfBy :: (a -> a-> Bool) ->a -> [a] -> Int
lastIndexOfBy eq n  =  maximum . map fst . filter (eq n . snd) . zip [0..]

getLines   :: IO [String]
getLines   =  do leesRegel <-  getLine
                 if   null leesRegel
                    then return  []
                    else do leesMeer <- getLine
                            return (leesRegel:leesMeer)

-- getLine should be: getLines
-- 18,41-47
