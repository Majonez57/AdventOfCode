--I hate this so much but shhh
follow :: (Int, Int) -> (Int, Int) -> (Int, Int)
follow (tx, ty) (hx, hy)  = let dx = hx - tx
                                dy = hy - ty
                            in case (dx, dy) of
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
                                _        -> (tx, ty)

moveH :: (Int, Int) -> String -> (Int, Int)
moveH (x, y) dir = case dir of
                        "R" -> (x+1, y)
                        "L" -> (x-1, y)
                        "U" -> (x , y+1)
                        "D" -> (x , y-1)
                        _   -> (0,0)

execMove :: ((Int, Int), (Int, Int), [(Int, Int)]) -> String -> Int -> ((Int,Int), (Int, Int), [(Int, Int)])
execMove (h,t,v) dir mag = let newH  = moveH h dir
                               newT  = follow t newH
                               newV  = if newT `elem` v then v else v ++ [newT]
                           in if mag == 1 then (newH, newT, newV) else execMove (newH, newT, newV) dir (mag-1)
                           
apply :: ((Int, Int), (Int, Int), [(Int, Int)]) -> [[String]] -> ((Int, Int), (Int, Int), [(Int, Int)])
apply a ((dir:mag:_):xs) = apply (execMove a dir (read mag)) xs
apply a [] = a

main :: IO ()
main = do
        content <- readFile "day9.txt"
        let xs = map words $ lines content
            res@(h,t,v) = apply ((0,0), (0,0), [(0,0)]) xs
        print(length v)
        return ()