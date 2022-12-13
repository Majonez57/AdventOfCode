import Data.List (elemIndex)
import Data.Maybe (fromJust)
data Packet = I Int
            | L [Packet]
            deriving (Show, Eq)

splitList :: (String, String) -> Int -> (String, String)
splitList (x:xs, ys) n = case x of
                                '[' -> if n== 0 then splitList(xs, ys) (n+1) else splitList(xs, ys ++ [x]) (n+1)
                                ']' -> if (n-1) == 0 then (xs, ys ++ [x]) else splitList(xs, ys ++ [x]) (n-1)
                                _   -> splitList(xs, ys ++ [x]) n
splitList ([], ys) _ = ([], ys)

toPacket :: String -> [Packet]
toPacket ('1':'0':xs) = I 10 : toPacket xs
toPacket (',':xs) = toPacket xs
toPacket a@('[':_) = L (toPacket s') : toPacket s
                    where (s,s') = splitList (a, []) 0
toPacket (']':xs) = toPacket xs
toPacket (n:xs) = I (read [n]) : toPacket xs
toPacket [] = []

helper :: [Packet] -> Packet
helper [] = L []
helper xs = head xs

order :: (Packet,Packet) -> Int
order (L (I a : as),L (I b : bs))     | a < b = 1
                                          | a > b = 0
                                          | otherwise = order (L as, L bs)
order (L (L as : ass), L (L bs : bss)) = if o == 2 then order (L ass, L bss) else o
                                        where o = order (L as, L bs)
order (L (L as : ass), L (I b :bs)) = order (L (L as : ass), L (L [I b]:bs))
order (L (I a  : as), L (L bs : bss)) = order (L (L [I a]:as), L (L bs : bss))
order (L [],L bs)   = if null bs then 2 else 1
order (L as, L [])  = if null as then 2 else 0

pairs :: [a] -> [(a,a)]
pairs (a:b:_:ds) = (a,b) : pairs ds
pairs [a,b] = [(a, b)]
pairs _ = []

bubble :: [Packet] -> [Packet]
bubble = foldr swap []

swap :: Packet -> [Packet] -> [Packet]
swap x [] = [x]
swap x (y:xs) = if order (x,y) == 1 then x : swap y xs
                                    else y : swap x xs

main :: IO()
main = do content <- readFile "day13.txt"
          let xs = lines content
              hs = map (helper. toPacket) xs
              p = map order (pairs hs)
              hs2 = map (helper . toPacket) (filter (/= []) xs)
              sorted = bubble (L [L [I 2]] : L [L [I 6]] : hs2)
              i2 = fromJust (elemIndex (L [L [I 2]]) sorted)
              i6 = fromJust (elemIndex (L [L [I 6]]) sorted)
          print(sum (zipWith (*) p [1..]))
          print((i2+1)*(i6+1))