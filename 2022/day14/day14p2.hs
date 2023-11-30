{-# LANGUAGE TupleSections #-}

import Data.Maybe (isNothing)
import Data.Map as Map (Map, fromList, lookup, insert, keys, union)

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

bottom :: Map Pos Tile -> Int
bottom = maximum . map snd . keys

getBot :: Map Pos Tile -> Map Pos Tile
getBot grd = union grd $ fromList $ map (,'#') (map (, b) [(100 - (2 * b) -1)..(100 + (2 * b) +1)])
           where b = bottom grd + 2

dropSand :: Pos -> Pos -> Map Pos Tile -> (Pos, Pos, Map Pos Tile)
dropSand fs@(x,y) ls ts | y == 0 && x == 100 && ls /= fs = (fs, ls, ts)
                        | isNothing n1          = dropSand (x, y+1) ls ts
                        | isNothing n2          = dropSand (x-1, y+1) ls ts
                        | isNothing n3          = dropSand (x+1, y+1) ls ts
                        | otherwise             = (fs, ls, insert fs 'o' ts)
                        where n1 = Map.lookup (x,   y+1) ts
                              n2 = Map.lookup (x-1, y+1) ts
                              n3 = Map.lookup (x+1, y+1) ts

findS :: Pos -> Map Pos Tile -> Int -> Int
findS ps ts n = if y == 0 && x == 100    then n
                                         else findS next mm (n+1)
              where (at@(x, y), next, mm) = dropSand ps ps ts

main :: IO()
main = do content <- readFile "mini.txt"
          let xs = map words (lines content)
              ins = map (allRocks . parseInput) xs
              initm = getBot $ Map.fromList $ initMap (concat ins)
          --print(initm)
          --print(fst $ dropSand (100,0) (100, 0) initm)
          print(findS (100, 0) initm 0)
          return()