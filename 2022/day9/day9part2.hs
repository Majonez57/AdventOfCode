--It got worse...
follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (hx, hy) (tx, ty)  = let dx = hx - tx 
                                dy = hy - ty
                            in if dx /= 2 && dy /= 2 then (tx, ty) -- SPEED 
                                                     else case (dx, dy) of
                                                            (2,   0) -> (tx + 1, ty)
                                                            (-2,  0) -> (tx - 1, ty)
                                                            (0,   2) -> (tx, ty + 1)
                                                            (0,  -2) -> (tx, ty - 1)
                                                            (1,   2) -> (tx+1, ty+1)
                                                            (1,  -2) -> (tx+1, ty-1)
                                                            (-1,  2) -> (tx-1, ty+1)
                                                            (-1, -2) -> (tx-1, ty-1)
                                                            (2,   1) -> (tx+1, ty+1)
                                                            (2,  -1) -> (tx+1, ty-1)
                                                            (-2,  1) -> (tx-1, ty+1)
                                                            (-2, -1) -> (tx-1, ty-1)
                                                            (2 ,  2) -> (tx+1, ty+1)
                                                            (-2, -2) -> (tx-1, ty-1)
                                                            (-2,  2) -> (tx-1, ty+1)
                                                            (2,  -2) -> (tx+1, ty-1)
                                                            _        -> (tx, ty)                     

moveH :: (Int, Int) -> String -> (Int, Int)
moveH (x, y) dir = case dir of
                        "R" -> (x+1, y)
                        "L" -> (x-1, y)
                        "U" -> (x , y+1)
                        "D" -> (x , y-1)
                        _   -> (0,0)
                     
bigFollow :: [(Int, Int)] -> [(Int, Int)]
bigFollow (x:y:xs) = let n = follow x y 
                     in n : bigFollow (n:xs)
bigFollow _ = []

execMove :: ([(Int, Int)], [(Int, Int)]) -> String -> Int -> ([(Int,Int)], [(Int, Int)])
execMove (h:ts,v) dir mag = let newH  = moveH h dir
                                newT  = bigFollow (newH:ts)
                                newV  = if last newT `elem` v then v else last newT:v
                           in if mag == 1 then (newH:newT, newV) else execMove (newH:newT, newV) dir (mag-1)
execMove _ _ _ = ([],[])


apply :: ([(Int, Int)], [(Int, Int)]) -> [[String]] -> ([(Int, Int)], [(Int, Int)])
apply a ((dir:mag:_):xs) = apply (execMove a dir (read mag)) xs
apply a [] = a

main :: IO ()
main = do
        content <- readFile "day9.txt"
        let xs = map words $ lines content
            (_,v) = apply (replicate 2 (0,0), [(0,0)]) xs
        print(length v)
        return ()