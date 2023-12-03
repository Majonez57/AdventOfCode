import Data.Char (isDigit)

isGear :: Char -> Bool
isGear x = x == '*'

isIn :: [((Int, Int), [Int])] -> (Int, Int) -> Int -> Int
isIn [] _ _ = -1
isIn ((xy, _):xs) xxyy i = if xy == xxyy then i
                           else isIn xs xxyy (i+1)

place :: [((Int, Int), [Int])] -> (Int, Int) -> Int -> [((Int, Int), [Int])]
place xss xxyy n = if i == -1 then xss ++ [(xxyy, [n])]
                              else take i xss ++ [(xxyy, n : snd (xss !! i))] ++ drop (i+1) xss

                  where i = isIn xss xxyy 0

mplace :: [((Int, Int), [Int])] -> [(Int, Int)] -> Int -> [((Int, Int), [Int])]
mplace xss (xxyy:xs) i = mplace (place xss xxyy i) xs i
mplace xss [] _ = xss

mapcoord :: [Bool] -> (Int, Int) -> [(Int, Int)]
mapcoord (True : xs) (x,y)= (x,y) : mapcoord xs (x+1, y)
mapcoord (False: xs) (x,y)= mapcoord xs (x+1, y)
mapcoord [] _ = []

total :: [String] -> (Int, Int) -> [((Int, Int), [Int])] -> [((Int, Int), [Int])]
total xss (x,y) n |x == xmax-1 && y == ymax-1 = n 
                  |nlen /= 0 && not (null gears) = total xss nextcoordn new
                  |otherwise                     = total xss nextcoord n

                  where num  = takeWhile isDigit xs
                        nlen = length num
                        rest = dropWhile isDigit xs

                        xmax = length (head xss)
                        ymax = length xss

                        above = take (nlen+2) (drop (x-1) (xss !! (y-1)))
                        below = take (nlen+2) (drop (x-1) (xss !! (y+1)))
                        right = head rest 
                        left  = (xss !! y) !! (max 0 x-1)

                        cabove = mapcoord (map isGear above) (x-1, y-1)
                        cbelow = mapcoord (map isGear below) (x-1, y+1)

                        gears = cabove ++ cbelow ++ ([(x + nlen, y) | isGear right]) ++ ([(x - 1, y) | isGear left])

                        new = mplace n gears (read num)

                        nl = x == xmax-1
                        nextcoordn = if nl then (0, y+1) else (x+nlen, y)
                        nextcoord  = if nl then (0, y+1) else (x+1, y)

                        xs = drop x (xss !! y)

fsum :: [((Int, Int), [Int])] -> Int
fsum ((_,ns):xs) = if length ns == 2 then product ns + fsum xs
                   else fsum xs
fsum [] = 0

main :: IO ()
main = do
        content <- (readFile "day3.txt")
        let allc = lines content
            newc = map (\x -> '.' : x ++ ['.']) allc
            len = length (head newc)
            new = [replicate len '.'] ++ newc ++ [replicate len '.']
            tot = total new (0,0) []
            fs = fsum tot
            
        
        print(fs)
        return()