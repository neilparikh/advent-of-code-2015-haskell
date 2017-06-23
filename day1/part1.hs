main = do
    input <- getLine
    print . solve $ input

solve :: String -> Int
solve = foldl (\acc c -> (if (c == '(') then succ else pred) acc) 0
