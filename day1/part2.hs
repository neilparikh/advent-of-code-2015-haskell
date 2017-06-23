main = do
    input <- getLine
    print . solve $ input

solve :: String -> Int
solve = snd . (foldl move (0, 0)) . (zip [1..])
    where
    move (floor, index) (i, c) = if (floor == -1) then (floor, index) else (nextAcc floor c, i)
    nextAcc acc c = (if (c == '(') then succ else pred) acc
