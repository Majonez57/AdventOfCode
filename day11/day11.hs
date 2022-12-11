import Data.Char
import Language.Haskell.TH (appT)
data Monkey = Monkey {
            items :: [Int],
            look  :: Int -> Int,
            check :: Int -> Bool,
            m1    :: Int,
            m2    :: Int,
            activ :: Int
}

parseMonkeys :: [[String]] -> [Monkey] -> [Monkey]
parseMonkeys (("Monkey":_):ss) ms = parseMonkeys ss (Monkey [] (+0) (==0) 0 0 0: ms)
parseMonkeys (("Starting":"items:":ns):ss) (m:ms) = parseMonkeys ss (m':ms)
                                                  where m' = m {items = map (read . take 2) ns}
parseMonkeys (("Operation:":"new":"=":"old":op:n:_):ss) (m:ms) = parseMonkeys ss (m':ms)
                                                  where m' = m {look = f}
                                                        f x = if n == "old" then (x^2) `div` 3 else (op' x n') `div` 3
                                                        op' = case op of
                                                                "*" -> (*)
                                                                "+" -> (+)
                                                                "-" -> (-)
                                                                "/" -> div
                                                        --n' = if isDigit (head n) then read n else 0
                                                        n' = read n
parseMonkeys (("Test:":"divisible":"by":n:_):ss) (m:ms) = parseMonkeys ss (m':ms)
                                                    where m' = m {check = f}
                                                          f x = (x `mod` read n) == 0
parseMonkeys (("If":"true:":"throw":"to":"monkey":n:_):ss) (m:ms) = parseMonkeys ss (m':ms)
                                                    where m' = m {m1 = read n}
parseMonkeys (("If":"false:":"throw":"to":"monkey":n:_):ss) (m:ms) = parseMonkeys ss (m':ms)
                                                    where m' = m {m2 = read n}
parseMonkeys ([]:ss) ms = parseMonkeys ss ms
parseMonkeys [] ms = ms

appNRound :: Int -> [Monkey] -> [Monkey]
appNRound n ms = if n==0 then ms else appNRound (n-1) (appRound 0 ms)

appRound :: Int -> [Monkey] -> [Monkey]
appRound n ms = if n == length ms then ms else appRound (n+1) (appTurn n ms)

appTurn :: Int -> [Monkey] -> [Monkey]
appTurn mi ms = insertAt m' mi thr
              where m = ms !! mi
                    m' = m {items = [], activ = activ m + length new}
                    new = map (look m) (items m)
                    thr = throwAll (map (checki (check m) (m1 m, m2 m)) new ) ms

checki :: (Int -> Bool) -> (Int, Int) -> Int -> (Int, Int)
checki f (tru, fal) val = if app then (tru, val) else (fal, val)
                          where app = f val

throwAll :: [(Int,Int)] -> [Monkey] -> [Monkey]
throwAll ((n, val):ts) ms = throwAll ts (throw n val ms)
throwAll [] ms = ms

throw :: Int -> Int -> [Monkey] -> [Monkey]
throw n val ms = insertAt m' n ms
                where m  = ms !! n 
                      m' = m {items =items m ++ [val]}

printM :: Monkey -> IO()
printM m = do
            print (items m)
            print (activ m)

main :: IO()
main = do
        content <- readFile "mini.txt"
        let xs = map words $ lines content
            ms = reverse (parseMonkeys xs [])
            t1 = appNRound 20 ms
        printM (head t1)
        printM (t1 !! 1)
        printM (t1 !! 2)
        printM (t1 !! 3)
        printM (t1 !! 4)
        printM (t1 !! 5)
        printM (t1 !! 6)
        printM (t1 !! 7)
        return ()

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 (a:as) = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as