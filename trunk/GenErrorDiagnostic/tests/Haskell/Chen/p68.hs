module Example where

import Data.Char

-- Problem: doRow r ys should be doRow ys r
plot f dx dy oy =                                                   
    let fxs = getYs f dx
        ys = map (\y-> fromIntegral (y-oy)*dy) [maxY,maxY-1..minY]
        rows = map (doRow fxs) ys
    in unlines rows
        where
        doRow [] r = ""
        doRow (y:ys) r = (if y < r && y > (r-dy) then '*'
                            else ' ') : doRow r ys
        getYs f dx = [ f ((centre x * dx)) | x <- [minX..maxX] ]
            where centre = (+) .5
minX = 0
maxX = 79
minY = 0
maxY = 19

-- 14,41-50
