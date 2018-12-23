module Days.Day18(run) where

import Data.Array
import qualified Data.Map as Map
import Data.Map (Map)

type Position = (Int, Int)
type Grid = Array Position Char

run :: String -> String
run s = show (checksum $ runMinutes (parse s) 10) ++ ", " ++ show (part2 (parse s))

parse :: String -> Grid
parse = listArray ((1, 1), (50, 50)) . concat . lines

neighbors :: Grid -> Position -> [Char]
neighbors grid (x, y) = map (get grid) [(x+1, y), (x-1, y), (x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1), (x, y+1), (x, y-1)]

get :: Grid -> Position -> Char
get grid (x, y) | x >= 1 && x <= 50 && y >= 1 && y <= 50 = grid ! (x, y)
                | otherwise = '.'

update :: Char -> [Char] -> Char
update '.' ns | length (filter (== '|') ns) >= 3 = '|'
              | otherwise = '.'
update '|' ns | length (filter (== '#') ns) >= 3 = '#'
              | otherwise = '|'
update '#' ns | elem '#' ns && elem '|' ns = '#'
              | otherwise = '.'

updateGrid :: Grid -> Grid
updateGrid grid = array (bounds grid) [(pos, update c (neighbors grid pos)) | (pos, c) <- assocs grid]

fpow :: Int -> (a -> a) -> (a -> a)
fpow n f = foldr (.) id $ replicate n f

runMinutes :: Grid -> Int -> Grid
runMinutes grid n = fpow n updateGrid grid

checksum :: Grid -> Int
checksum grid = (length $ filter (== '|') $ elems grid) * (length $ filter (== '#') $ elems grid)

runWithHistory :: Map String Int -> Grid -> Int -> (Int, Int)
runWithHistory history grid turn = let updatedGrid = updateGrid grid
                                       value = elems updatedGrid
                                         in case Map.lookup value history of
                                              Nothing -> runWithHistory (Map.insert value turn history) updatedGrid (turn+1)
                                              Just begin -> (begin, turn - begin)

part2 :: Grid -> Int
part2 grid = let (begin, period) = runWithHistory Map.empty grid 1 in
                   checksum $ runMinutes grid (begin + ((1000000000 - begin) `mod` period))
                                  
