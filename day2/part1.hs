-- requires the following cabal packages
--   split

import Data.List.Split (splitOn)

main = do
    input <- getContents
    let packages = map (listToTuple . (map read) . splitOn "x") . lines $ input
    print . sum . map areaForPackage $ packages

areaForPackage :: (Int, Int, Int) -> Int
areaForPackage (x, y, z) = minArea + (foldl (\acc area -> acc + 2 * area) 0 faces)
    where
    faces = [x * y, y * z, x * z]
    minArea = minimum faces

listToTuple :: [a] -> (a, a, a)
listToTuple [x, y, z] = (x, y, z)
listToTuple _ = error "shouldn't happen"
