-- requires the following cabal packages
--   split

import Data.List.Split (splitOn)

main = do
    input <- getContents
    let packages = map (listToTuple . (map read) . splitOn "x") . lines $ input
    print . sum . map ribbonForPackage $ packages

ribbonForPackage :: (Int, Int, Int) -> Int
ribbonForPackage (x, y, z) = minPerimeter + x * y * z
    where
    perimeters = [perimeter x y, perimeter y z, perimeter x z]
    perimeter a b = 2 * a + 2 * b
    minPerimeter = minimum perimeters

listToTuple :: [a] -> (a, a, a)
listToTuple [x, y, z] = (x, y, z)
listToTuple _ = error "shouldn't happen"
