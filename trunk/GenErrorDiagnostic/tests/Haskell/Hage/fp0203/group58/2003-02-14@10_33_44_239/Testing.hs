fac :: Float -> Float

fac n   |       n < 0.9    = 1.0
        |       otherwise = n * fac (n - 1.0)


blaat   ::      String
blaat   =       "Public Static main(String args[]) void"

main :: IO()
main = do
     putStr "GIVE ME YOUR NAME!! "
     naam <- getLine
     putStrLn ("Hallo, " ++ naam ++ ".")
     blaat

-- should be: putStr blaat
-- 15,6-10
