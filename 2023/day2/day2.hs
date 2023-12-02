
conv :: [[Int]] -> Int -> [Int]
conv ((b:g:r:_):xs) x = (if or [b > 14, g > 13, r > 12] then 0 else x) : conv xs (x+1)
conv _ _ = []

conv2 :: [[Int]] -> [Int]
conv2 ((b:g:r:_):xs) = b*g*r : conv2 xs 
conv2 _ = []

colours :: [String] -> [Int] -> [Int]
colours (n:"blue":xs) (b:g:r:_)= colours xs [max b (read n), g, r] 
colours (n:"red":xs) (b:g:r:_)= colours xs [b, g, max r (read n)] 
colours (n:"green":xs) (b:g:r:_)= colours xs [b, max g (read n), r] 
colours _ xs = xs

byecommas :: String -> String
byecommas "" = ""
byecommas (',':xs) = byecommas xs
byecommas (';':xs) = byecommas xs
byecommas (x:xs) = x : byecommas xs

poss :: [String] -> [Int]
poss ("Game":_:xs) = colours xs [0,0,0]
poss _ = []
--poss ()

main :: IO ()
main = do
        content <- (readFile "day2.txt")
        let linez = lines content
            nocom = map byecommas linez
            cols  = map (poss.words) nocom
            total = sum $ conv cols 1
            total2 = sum $ conv2 cols

        print total
        print total2
        return()