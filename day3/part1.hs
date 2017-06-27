import ListZipper
import Grid

main = do
    input <- getLine
    print . solve $ input

solve :: String -> Int
solve = snd . foldl (\(grid, count) dir -> (move dir grid, newCount grid count)) (initialGrid, 0)
    where
    newCount :: Grid -> Int -> Int
    newCount grid count = if (getCurrent (getCurrent grid)) == 1 then count + 1
                                                                 else count

move :: Char -> Grid -> Grid
move '^' grid = updateCurrent (updateCurrent succ) . goRight $ grid
move 'v' grid = updateCurrent (updateCurrent succ) . goLeft  $ grid
move '>' (ListPath down curr up) = ListPath (map goRight down) (updateCurrent succ . goRight $ curr) (map goRight up)
move '<' (ListPath down curr up) = ListPath (map goLeft  down) (updateCurrent succ . goLeft  $ curr) (map goLeft  up)
move _  grid = grid
