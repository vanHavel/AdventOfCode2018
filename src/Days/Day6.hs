module Days.Day6 where 

import Data.Array
import Data.Ord
import Data.List

type Position = (Int, Int)

run :: String -> String
run s = let arr = parse s 
          in show (biggestFiniteIndex arr) ++ ", " ++ show (length $ internalRegion arr)

parse :: String -> Array Int Position
parse = makeArray . map toPos . map parseLine . lines 
  where parseLine = map read . words . filter (/= ',')
        toPos [x, y] = (x, y)
        makeArray xs = listArray (1, length xs) xs

-- part 1
biggestFiniteIndex :: Array Int Position -> Int
biggestFiniteIndex arr = maximum [length . filter (==i) $ nearestIndices arr | i <- finitePositionIndices arr]

nearestIndices :: Array Int Position -> [Int]
nearestIndices arr = let (minx, maxx, miny, maxy) = grid arr
                        in map (flip nearestPositionIndex $ arr) [(x, y) | x <- [minx..maxx], y <- [miny..maxy]]

nearestPositionIndex :: Position -> Array Int Position -> Int
nearestPositionIndex pos arr = argminIfUnique [distance p pos | p <- elems arr]

finitePositionIndices :: Array Int Position -> [Int]
finitePositionIndices arr = filter (neverNearBorder arr) [1..(length $ elems arr)]

neverNearBorder :: Array Int Position -> Int -> Bool
neverNearBorder arr i = all (\pos -> nearestPositionIndex pos arr /= i) (border arr)

border :: Array Int Position -> [Position]
border arr = let (minx, maxx, miny, maxy) = grid arr
                in (zip [minx..maxx] (repeat miny)) ++ (zip [minx..maxx] (repeat maxy)) 
                    ++ (zip (repeat minx) [miny..maxy]) ++ (zip (repeat maxx) [miny..maxy])

grid :: Array Int Position -> (Int, Int, Int, Int)
grid arr = let minx = minimum $ map fst $ elems arr
               maxx = maximum $ map fst $ elems arr
               miny = minimum $ map snd $ elems arr
               maxy = maximum $ map snd $ elems arr
                 in (minx, maxx, miny, maxy)

argminIfUnique :: (Ord a) => [a] -> Int
argminIfUnique xs | minimum xs == (sort xs) !! 1 = -1
                  | otherwise                      = argmin xs

argmin :: (Ord a) => [a] -> Int
argmin = fst . minimumBy (comparing snd) . zip [1..]

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- part 2
internalRegion :: Array Int Position -> [Position]
internalRegion arr = let (minx, maxx, miny, maxy) = grid arr
  in [(x, y) | x <- [minx..maxx], y <- [miny..maxy], (sum (map (distance (x, y)) $ elems arr)) < 10000]