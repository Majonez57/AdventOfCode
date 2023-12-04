
wins :: ([Int], [Int]) -> Int
wins (w, n) = (length . filter (\x -> (elem x) n) ) w

parser :: [String] -> ([Int], [Int])
parser ("Card":n:xs) = parser xs
parser xs = (map read win,map read $ tail num)
        where win = takeWhile (/= "|") xs
              num = dropWhile (/= "|") xs
              len = length win

inc :: [(Int, Int)] -> Int -> [(Int, Int)]
inc ((v,t):xs) i = (v, t+i) : inc xs i
inc [] _ = []

recurse :: ([(Int,Int)],Int) -> [(Int, Int)] -> [(Int, Int)]
recurse (ws, i) wss = if i == length ws then wss
                     else recurse (new, i+1) new
                      where 

                            new = take (i+1) ws ++ inc change times ++ drop (i+ 1 + val) ws

                            change = take val (drop (i+1) ws)

                            w = ws !! i
                            times = snd w
                            val   = fst w

recurseh :: [Int] -> [(Int, Int)]
recurseh ws = recurse (l, 0) []
            where l = zip ws (replicate (length ws) 1)

final :: [(Int, Int)] -> Int
final ((x,n):xs) = n + final xs
final [] = 0

main :: IO ()
main = do
        content <- (readFile "day4.txt")
        let allc = lines content
            wor = map words allc
            par = map parser wor
            winz = map wins par
            tot = map (\x -> if x > 0 then 2^(x-1) else 0) winz
            tot2 = final (recurseh winz)
            
        
        print(tot)
        print(tot2)
        return()