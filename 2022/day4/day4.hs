
isContained :: [Int] -> Int
isContained (a:b:c:d:_) = if ((a >= c) && (b <= d)) || ((a <= c) && (d <= b)) then 1 else 0

isContained2 :: [Int] -> Int
isContained2 (a:b:c:d:_) = if (b < c) || (d < a) then 0 else 1

-- PLEASE OVERLOOK THIS JANKY SOLUTION, IT WONT LET ME IMPORT SPLIT!
removeSplits :: String -> [Int]
removeSplits cs = map read $ words [if c == ',' || c == '-' then ' ' else c|c <- cs]

main :: IO ()
main = do
        content <- readFile "day4.txt"
        let xs = lines content
        print(sum $ map (isContained  . removeSplits) xs)
        print(sum $ map (isContained2 . removeSplits) xs)
