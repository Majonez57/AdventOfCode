{-# LANGUAGE TupleSections #-}

import Data.Maybe (isNothing)
import Data.Map as Map (Map, fromList, lookup, insert)

type Pos = (Int, Int)
type Tile = Char

parseInput :: [String] -> [Pos]
parseInput ("->":xss) = parseInput xss
parseInput (co:xss) = (x, y) : parseInput xss
                    where x = read (takeWhile (/= ',') co) - 400
                          y = read $ tail (dropWhile (/= ',') co)
parseInput [] = []

allRocks :: [Pos] -> [Pos]
allRocks ((x,y):(x2, y2):xs) = init new ++ allRocks ((x2, y2):xs)
                             where new = if x == x2 then zip (repeat x) rangey
                                                    else zip rangex (repeat y)
                                   rangex = if x < x2 then [x..x2] else reverse [x2..x]
                                   rangey = if y < y2 then [y..y2] else reverse [y2..y]
allRocks a = a

initMap :: [Pos] -> [(Pos, Tile)]
initMap = map (,'#')

dropSand :: Pos -> Pos -> Map Pos Tile -> (Pos, Map Pos Tile)
dropSand fs@(x,y) ls ts | y == 200      = (fs, ts)
                        | isNothing n1  = dropSand (x, y+1) ls ts
                        | isNothing n2  = dropSand (x-1, y+1) ls ts
                        | isNothing n3  = dropSand (x+1, y-1) ls ts
                        | otherwise     = (ls, insert fs 'o' ts)
                        where n1 = Map.lookup (x,   y+1) ts
                              n2 = Map.lookup (x-1, y+1) ts
                              n3 = Map.lookup (x+1, y+1) ts

findS :: Pos -> Map Pos Tile -> Int -> Int
findS ps ts n = if y == 200 then n
                            else findS next mm (n+1)
              where (next@(_,y), mm) = dropSand ps ps ts

main :: IO()
main = do content <- readFile "mini.txt"
          let xs = map words (lines content)
              ins = map (allRocks . parseInput) xs
              initm = Map.fromList $ initMap (concat ins)
          --print(initm)
          --print(dropSand (100,0) (100, 0) initm)
          print(findS (100, 0) initm 0)
          return()