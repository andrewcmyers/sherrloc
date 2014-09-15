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

-- "" should be []; x:y should be (x:y)
-- 9,44-45   9,37-45   12,40-49   12,47-49
