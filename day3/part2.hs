import ListZipper
import Grid

main = do
    input <- getLine
    let (santa, robo) = alternateSplit input ([], [])
    let (newGrid, count) = solve santa (initialGrid, 0) -- move santa and get a new grid and count
    let gridAtZeroZero = foldl (flip move) newGrid (reversePath santa) -- move the zipper back to (0, 0)
    let gridAtZeroZero' = incrCurrent gridAtZeroZero -- increment (0,0) so it's not double counted
    print $ snd $ solve robo (gridAtZeroZero', count) -- move robo-santa and get a total count

solve :: String -> (Grid, Int) -> (Grid, Int)
solve = flip $ foldl (\(grid, count) dir -> (moveAndIncr dir grid, newCount grid count))
    where
    newCount :: Grid -> Int -> Int
    newCount grid count = if (getCurrent (getCurrent grid)) == 1 then count + 1
                                                                 else count

moveAndIncr :: Char -> Grid -> Grid
moveAndIncr c = incrCurrent . (move c)

move :: Char -> Grid -> Grid
move '^' grid = goRight $ grid
move 'v' grid = goLeft  $ grid
move '>' (ListPath down curr up) = ListPath (map goRight down) (goRight curr) (map goRight up)
move '<' (ListPath down curr up) = ListPath (map goLeft  down) (goLeft  curr) (map goLeft  up)
move _  grid = grid

incrCurrent :: Grid -> Grid
incrCurrent = updateCurrent (updateCurrent succ)

reversePath :: String -> String
reversePath = reverse . map flipDir
    where
    flipDir '^' = 'v'
    flipDir 'v' = '^'
    flipDir '<' = '>'
    flipDir '>' = '<'
    flipDir x   = x


alternateSplit :: [a] -> ([a], [a]) -> ([a], [a])
alternateSplit [] (a, b) = (reverse a, reverse b)
alternateSplit [x] (a, b) = alternateSplit [] (x:a, b)
alternateSplit (x:y:xs) (a, b) = alternateSplit xs (x:a, y:b)
