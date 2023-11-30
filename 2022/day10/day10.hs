import Data.ByteString (split)

process :: [String] -> [Int] -> [Int]
process ("noop":_) xs = xs ++ [last xs]
process ("addx":('-':ns):_) tot = tot ++ [x, x - read ns]
                                where x = last tot
process ("addx":ns:_) tot = tot ++ [x, x + read ns]
                                where x = last tot
process [] tot = tot

generat :: [[String]] -> [Int] -> [Int]
generat (s:ss) xs = generat ss (process s xs)
generat [] xs = xs

getval :: [Int] -> [Int] -> [Int]
getval (i:is) xs = (xs !! (i-1))*i : getval is xs
getval [] _ = []

splitPix :: [[a]] -> [[a]]
splitPix xs = if null b then xs else a : splitPix [b]
                where (a,b) = splitAt 40 $ last xs

pixels :: (Int, Int) -> Char
pixels (x, m) = if x < n+2 && x > n-2 then '#' else '='
                where n = m `mod` 40

main :: IO()
main = do
        content <- readFile "day10.txt"
        let xs = map words $ lines content
            done = generat xs [1]
            values = getval [20,60,100,140,180,220] done
            split = zip (init done) [0..]
        print(sum values)
        print(splitPix [map pixels split])
        return()