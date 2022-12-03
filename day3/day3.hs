import Data.List (intersect)

getVal :: String -> Int
getVal xs = if val > 97 then val - 96 else val - 38
           where val = fromEnum (head (c1 `intersect` c2))
                 len = length xs
                 c1  = take (len `div` 2) xs
                 c2  = drop (len `div` 2) xs

main :: IO ()
main = do
        content <- readFile "day3.txt"
        let xs = lines content
        print(sum (map getVal xs))

--- PROBLEM 2

threes :: [a] -> [[a]]
threes (x:y:z:s) = [x, y, z] : threes s
threes _ = []

getVal2 :: [String] -> Int
getVal2 (x:y:z:_) = if val > 97 then val - 96 else val - 38
                  where val = fromEnum (head ((x `intersect` y) `intersect` z))

main2 :: IO ()
main2 = do
         content <- readFile "day3.txt"
         let xs = lines content
         print(sum (map getVal2 (threes xs)))