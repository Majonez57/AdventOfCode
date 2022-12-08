import Data.List (transpose)

andMat :: [[Int]] -> [[Int]] -> [[Int]]
andMat (a:as) (b:bs) = map (uncurry (*)) (zip a b) : andMat as bs
andMat [] _ = []
andMat _ [] = []

parseTrees :: String -> [Int]
parseTrees xs = [read [x] | x <- xs]

checkRow' :: Int -> [Int] -> Int
checkRow' x (t:ts) = if x > t then 1 + checkRow' x ts else 1
checkRow' _ [] = 0

checkRow :: [Int] -> [Int]
checkRow (t:ts) = checkRow' t ts : checkRow ts
checkRow [] = []

horView :: [Int] -> [Int]
horView ts = let left  = checkRow ts
                 right = reverse (checkRow (reverse ts))
              in map (uncurry (*)) (zip left right)

fullView :: [[Int]] -> [[Int]]
fullView ts = let hor = map horView ts
                  ver = transpose (map horView (transpose ts))
              in andMat hor ver

main :: IO ()
main = do
        content <- readFile "day8.txt"
        let xs = map parseTrees (lines content)
            aw = fullView xs
        print(maximum $ map maximum aw)
        return ()