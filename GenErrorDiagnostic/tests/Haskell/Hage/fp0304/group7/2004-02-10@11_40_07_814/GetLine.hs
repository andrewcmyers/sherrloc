module GetLine where

eqChar      :: Char -> Char -> Bool
eqChar = undefined

getLine2 :: String -> IO()
getLine2 (c:cs) | eqChar c '\n' = return []
                | otherwise = do putChar c
                                 getLine2 cs
getLine2 "\n" = return ()
getLine2 [] = return ()

-- should return ()
-- 7,35-43   7,42-43
