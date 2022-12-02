
parseGame :: String -> Int
parseGame (p1:' ':p2) = case p1 of
                             'A' -> case p2 of -- Rock
                                        "X" -> 3 + 1
                                        "Y" -> 6 + 2
                                        "Z" -> 0 + 3
                             'B' -> case p2 of -- Paper
                                        "X" -> 0 + 1
                                        "Y" -> 3 + 2
                                        "Z" -> 6 + 3
                             'C' -> case p2 of -- Sciccors
                                        "X" -> 6 + 1
                                        "Y" -> 0 + 2
                                        "Z" -> 3 + 3

parseGame2 :: String -> Int
parseGame2 (p1:' ':p2) = case p1 of
                             'A' -> case p2 of -- Rock
                                        "Z" -> 6 + 2 -- Win is Paper
                                        "Y" -> 3 + 1 -- Draw is Rock
                                        "X" -> 0 + 3 -- Loss is Scissors
                             'B' -> case p2 of -- Paper
                                        "Z" -> 6 + 3
                                        "Y" -> 3 + 2
                                        "X" -> 0 + 1
                             'C' -> case p2 of -- Sciccors
                                        "Z" -> 6 + 1
                                        "Y" -> 3 + 3
                                        "X" -> 0 + 2

main :: IO ()
main = do
        content <- readFile "day2.txt"
        let xs = lines content
        print (sum (map parseGame xs))
        print (sum (map parseGame2 xs))
        return ()

