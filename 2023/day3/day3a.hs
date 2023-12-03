import Data.Char (isDigit)

isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && (x /= '.')


total :: [String] -> (Int, Int) -> [Int] -> [Int]
total xss (x,y) n |x == xmax-1 && y == ymax-1 = n 
                  |nlen /= 0 && count         = total xss nextcoordn (n ++ [read num])
                  |otherwise                  = total xss nextcoord n

                  where num  = takeWhile isDigit xs
                        nlen = length num
                        rest = dropWhile isDigit xs

                        xmax = length (head xss)
                        ymax = length xss

                        above = take (nlen+2) (drop (x-1) (xss !! (y-1)))
                        below = take (nlen+2) (drop (x-1) (xss !! (y+1)))
                        right  = head rest 
                        left = (xss !! y) !! (max 0 x-1)

                        around = above ++ below ++ [right] ++ [left]
                        count = any isSymbol around

                        nl = x == xmax-1
                        nextcoordn = if nl then (0, y+1) else (x+nlen, y)
                        nextcoord  = if nl then (0, y+1) else (x+1, y)

                        xs = drop x (xss !! y)

main :: IO ()
main = do
        content <- (readFile "day3.txt")
        let allc = lines content
            newc = map (\x -> '.' : x ++ ['.']) allc
            len = length (head newc)
            new = [replicate len '.'] ++ newc ++ [replicate len '.']
            tot = total new (0,0) []
            
        
        print(sum tot)
        return()