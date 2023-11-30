import Data.List (transpose)
andMat :: [[Bool]] -> [[Bool]] -> [[Bool]]
andMat (a:as) (b:bs) = map (uncurry (||)) (zip a b) : andMat as bs
andMat [] _ = []
andMat _ [] = []

parseTrees :: String -> [Int]
parseTrees xs = [read [x] | x <- xs]

checkRow' :: Int -> [Int] -> [Bool]
checkRow' c (t:ts) = if t > c then True:checkRow' t ts else False:checkRow' c ts
checkRow' _ [] = []

checkRow :: [Int] -> [Bool]
checkRow = checkRow' (-1)

horView :: [Int] -> [Bool]
horView ts = let left  = checkRow ts
                 right = reverse (checkRow (reverse ts))
              in map (\(a,b) -> a || b) (zip left right)

fullView :: [[Int]] -> [[Bool]]
fullView ts = let hor = map horView ts
                  ver = transpose (map horView (transpose ts))
              in andMat hor ver

countM :: (a -> Bool) -> [[a]] -> Int
countM f (x:xs) = length (filter f x) + countM f xs
countM _ [] = 0

main :: IO ()
main = do
        content <- readFile "day8.txt"
        let xs = map parseTrees (lines content)
            aw = fullView xs
        print(aw)
        print(countM (==True) aw)
        return ()