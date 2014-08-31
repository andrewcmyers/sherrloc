module IO where

eqChar      :: Char -> Char -> Bool
eqChar = undefined

getLn :: IO String
getLn = do
            x <- getChar
            if (eqChar x '\n') then return ""
                              else do
                                       y <- getLn
                                       return x:y

-- "" should be []
-- 9,44-45
