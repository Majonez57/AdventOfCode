getSize :: [[String]] -> Int
getSize (("dir":_):xss) = getSize xss
getSize ((s:_):xss)     = read s + getSize xss
getSize _ = 0 

createTree :: ([Int],[Int]) -> [[String]] -> ([Int],[Int])
createTree (stk, end) (("$":"cd":"..":_):xss) = createTree (tail stk, head stk : end) xss
createTree (stk, end) (("$":"cd":_):("$":"ls":_):xss) = createTree (map (getSize (takeWhile (\x -> head x /= "$") xss) +) (0:stk),end) (dropWhile (\x -> head x /= "$") xss)
createTree (stk, end) _ = (stk, end)

main :: IO()
main = do
        content <- readFile "day7.txt"
        let xs  = map words (lines content)
            (a, b) = createTree ([],[]) xs
            tot = filter (<= 100000) (a ++ b) -- All files smaller than 100000
            req = 30000000 - (70000000 - last a)
            rem = filter (>= req) (a++b)
        print(sum tot) -- Part 1
        print(minimum rem) -- Part 2
        return ()