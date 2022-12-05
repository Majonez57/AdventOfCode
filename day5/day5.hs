import Data.List (transpose)
import Data.Char (isLetter)

move :: [String] -> [Int] -> [String]
move xss (a:b:c:_) = if b > c then (take (c-1) xss) ++ [(stuff ++ to)] ++ (take (b-c-1) (drop (c) xss)) ++ [(drop a from)] ++ (drop (b) xss) 
                              else (take (b-1) xss) ++ [(drop a from)] ++ (take (c-b-1) (drop (b) xss)) ++ [(stuff ++ to)] ++ (drop (c) xss)
                    where from   = xss !! (b-1)
                          to     = xss !! (c-1)
                          stuff  = reverse (take a from)
move xss _ = xss

apply :: [String] -> [[Int]] -> [String]
apply xss (y:ys) = apply (move xss y) ys
apply xss [] = xss

tops :: [String] -> String
tops xss = concat [if xs == "" then "" else [head xs] | xs <- xss]

lazy :: [String] -> [Int]
lazy ("move":a:"from":b:"to":c:_) = map read [a,b,c]
lazy _ = []

main :: IO ()
main = do
        content <- (readFile "day5.txt")
        let allc = lines content
            crates = filter (/= "") $ map (filter isLetter) $ transpose (init (takeWhile (/= "") allc))
            instr  = tail $ map (lazy . words) $ drop (length crates) allc
        
        print(tops $ apply crates (instr))
        return()