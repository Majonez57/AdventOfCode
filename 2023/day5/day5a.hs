seeds :: [String] -> [Int]
seeds ("seeds:":xs) = [read x | x <- xs]
seeds _ = []

parsee :: [[String]] -> [[[Int]]]
parsee ([]:_:xs) = [map read ns | ns <- takeWhile (/= []) xs] : parsee (dropWhile (/= []) xs)
parsee _ = []

find :: [[Int]] -> Int -> Int
find ((a:b:c:_):xs) n = if n >= b && n <= (b+c+1) then a + (n - b)
                         else find xs n 
find [] n = n

fin :: [[[Int]]] -> Int -> Int
fin (l:ls) n = fin ls next
               where next = find l n
fin [] n = n

main :: IO ()
main = do
        content <- (readFile "test.txt")
        let allc = lines content
            word = map words allc

            seedz = (seeds . head) word

            parsez = parsee (tail word)
            tot1 =  map (fin parsez ) seedz


        print(minimum tot1)
        --print(minimum tot2)

        return()